/*
 * rcr.c: Redundant check removal
 *
 * Author:
 *   Massimiliano Mantione (massi@ximian.com)
 *   Gianluigi Spagnuolo
 *
 * (C) 2004 Ximian, Inc.  http://www.ximian.com
 */
#include <string.h>
#include <stdio.h>

#include <mono/metadata/debug-helpers.h>
#include <mono/metadata/mempool.h>
#include <mono/metadata/opcodes.h>

#include "inssel.h"
#include "rcr.h"


#define TRACE_RC_REMOVAL (verbose_level > 2)
#define REPORT_RC_REMOVAL (verbose_level > 0)

/*
 * A little hack for the verbosity level.
 * The verbosity level is stored in the cfg, but not all functions that must
 * print something see the cfg, so we store the verbosity level here at the
 * beginning of the algorithm.
 * This is not thread safe (does not handle correctly different verbosity
 * levels in different threads), and is not exact in case of dynamic changes
 * of the verbosity level...
 * Anyway, this is not needed, all that can happen is that something more
 * (or less) is logged, the result is in any case correct.
 */
static int verbose_level;


#define RELATION_BETWEEN_VALUES(value,related_value) (\
	((value) > (related_value))? MONO_GT_RELATION :\
	(((value) < (related_value))? MONO_LT_RELATION : MONO_EQ_RELATION))

#define MONO_INST_ARITY(inst) do{\
        if (mono_burg_arity[(inst)->opcode]) {\
			process_inst ((inst)->inst_left, bb, area, result, bb_scope, MONO_UNKNOWN_CONTEXT);\
		    if (mono_burg_arity[(inst)->opcode] > 1) {\
			   process_inst ((inst)->inst_right, bb, area, result, bb_scope, MONO_UNKNOWN_CONTEXT);\
			}\
		}\
	} while(0)
	
#define MAKE_VALUE_ANY(v) do{\
		(v).type = MONO_EMPTY_RELATION;\
	} while (0)

	
#define MONO_NEGATED_RELATION(r) ((~(r))&MONO_ANY_RELATION)
#define MONO_SYMMETRIC_RELATION(r) (((r)&MONO_EQ_RELATION)|(((r)&MONO_LT_RELATION)<<1)|((r&MONO_GT_RELATION)>>1))


static void
print_relation (int relation) {
	int print_or = 0;
	printf ("(");
	if (relation & MONO_LT_RELATION) {
		printf ("LT");
		print_or = 1;
	}
	if (relation & MONO_EQ_RELATION) {
		if (print_or) {
			printf ("|");
		}
		printf ("EQ");
		print_or = 1;
	}
	if (relation & MONO_GT_RELATION) {
		if (print_or) {
			printf ("|");
		}
		printf ("GT");
		print_or = 1;
	}
	printf (")");
}

static void
print_class_relation (int relation) {
	printf ("(");
	if (relation == MONO_IS_ISTANCE_OF) {
		printf ("IS ISTANCE");
	} else {
		printf ("IS NOT ISTANCE");
	}
	printf (")");
}

static void
print_summarized_value (MonoKnownFact *fact) {
	switch (fact->type) {
		case MONO_EMPTY_RELATION:
		printf ("value EMPTY");
		break;
	case MONO_CONSTANT_RELATION:
		printf ("value CONSTANT %d", fact->value.constant.value);
		break;
	case MONO_VARIABLE_RELATION:
		printf ("value VARIABLE %d, delta %d", fact->value.variable.variable, fact->value.variable.delta);
		break;
	case MONO_PHI_RELATION: {
		int phi;
		printf ("value PHI (");
		for (phi = 0; phi < fact->value.phi.number_of_alternatives; phi++) {
			if (phi) printf (",");
			printf ("%d", fact->value.phi.phi_alternatives [phi]);
		}
		printf (")");
		break;
	}
	case MONO_CLASS_RELATION: {
		MonoClass *kl = fact->value.klass.klass;
		printf ("CLASS %s.%s", kl->name_space, kl->name);
	    break;
	}
	default:
		g_assert_not_reached ();
	}
}

static void
print_summarized_value_relation (MonoKnownFact *known_fact) {
	printf ("Relation ");
	
	switch (known_fact->type) {
		case MONO_CONSTANT_RELATION:
			print_relation (known_fact->value.constant.relation);
		break;
		case MONO_VARIABLE_RELATION:
			print_relation (known_fact->value.variable.relation);
		break;
		case MONO_PHI_RELATION:
			print_relation (known_fact->value.phi.relation);
		break;
		case MONO_CLASS_RELATION:
			print_class_relation (known_fact->value.klass.relation);
		break;
		case MONO_EMPTY_RELATION:
			printf( "(NULL)" );
		break;
		default:
			g_assert_not_reached ();
	}
	
	printf (" with ");
	print_summarized_value (known_fact);
}

#if 0
static void
print_summarized_value_relation_chain (MonoKnownFact *known_fact) {
	printf ("Relations:\n");
	while (known_fact) {
		print_summarized_value_relation (known_fact);
		printf ("\n");
		known_fact = known_fact->next;
	}
}
#endif

static void
print_evaluation_context_status (MonoKnownFactsEvaluationStatus status) {
	if (status == MONO_KNOWNFACTS_EVALUATION_NOT_STARTED) {
		printf ("EVALUATION_NOT_STARTED");
	} else {
		gboolean print_or = FALSE;
		
		printf ("(");
		if (status & MONO_KNOWNFACTS_EVALUATION_IN_PROGRESS) {
			if (print_or) printf ("|");
			printf ("EVALUATION_IN_PROGRESS");
			print_or = TRUE;
		}
		if (status & MONO_KNOWNFACTS_EVALUATION_COMPLETED) {
			if (print_or) printf ("|");
			printf ("EVALUATION_COMPLETED");
			print_or = TRUE;
		}
		if (status & MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_ASCENDING) {
			if (print_or) printf ("|");
			printf ("RECURSIVELY_ASCENDING");
			print_or = TRUE;
		}
		if (status & MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_DESCENDING) {
			if (print_or) printf ("|");
			printf ("RECURSIVELY_DESCENDING");
			print_or = TRUE;
		}
		if (status & MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_INDEFINITE) {
			if (print_or) printf ("|");
			printf ("RECURSIVELY_INDEFINITE");
			print_or = TRUE;
		}
		printf (")");
	}
}


static void
print_evaluation_context_ranges (MonoKnownFactsEvaluationRanges *ranges) {
	printf ("(ranges: zero [%d,%d], variable [%d,%d])", ranges->zero.lower, ranges->zero.upper, ranges->variable.lower, ranges->variable.upper);
}

static void
print_evaluation_context (MonoKnownFactsEvaluationContext *context) {
	printf ("Context status: ");
	print_evaluation_context_status (context->status);
	if (context->status & (MONO_KNOWNFACTS_EVALUATION_IN_PROGRESS|MONO_KNOWNFACTS_EVALUATION_COMPLETED)) {
		print_evaluation_context_ranges (&(context->ranges));
	}
	printf ("\n");
}

#if 0
static void
print_evaluation_area (MonoKnownFactsEvaluationArea *area) {
	int i;
	printf ("Dump of evaluation area (%d variables):\n", area->cfg->num_varinfo);
	for (i = 0; i < area->cfg->num_varinfo; i++) {
		printf ("Variable %d: ", i);
		print_evaluation_context (&(area->contexts [i]));
		print_summarized_value_relation_chain (area->known_facts [i]);
	}
}

static void
print_evaluation_area_contexts (MonoKnownFactsEvaluationArea *area) {
	int i;
	printf ("Dump of evaluation area contexts (%d variables):\n", area->cfg->num_varinfo);
	for (i = 0; i < area->cfg->num_varinfo; i++) {
		printf ("Variable %d: ", i);
		print_evaluation_context (&(area->contexts [i]));
	}
}
#endif


/*
 * Check if the delta of an integer variable value is safe with respect
 * to the variable size in bytes and its kind (signed or unsigned).
 * If the delta is not safe, make the value an "any".
 */
static void
check_delta_safety (MonoKnownFactsEvaluationArea *area, MonoKnownFact *value) {
	
	if (value->type == MONO_VARIABLE_RELATION) {
		int variable = value->value.variable.variable;
		int delta = value->value.variable.delta;
		if ((area->variable_value_kind [variable]) & MONO_UNSIGNED_VALUE_FLAG) {
			if (delta < 0) {
				MAKE_VALUE_ANY (*value);
			}
		} else {
			if (((area->variable_value_kind [variable]) & MONO_INTEGER_VALUE_SIZE_BITMASK) < 4) {
				MAKE_VALUE_ANY (*value);
			} else if (delta > 16) {
				MAKE_VALUE_ANY (*value);
			}
		}
	}
}


static MonoValueRelation
get_relation_from_branch_instruction (int opcode) {
	switch (opcode) {
	case CEE_BEQ:
		return MONO_EQ_RELATION;
	case CEE_BLT:
	case CEE_BLT_UN:
		return MONO_LT_RELATION;
	case CEE_BLE:
	case CEE_BLE_UN:
		return MONO_LE_RELATION;
	case CEE_BGT:
	case CEE_BGT_UN:
		return MONO_GT_RELATION;
	case CEE_BGE:
	case CEE_BGE_UN:
		return MONO_GE_RELATION;
	case CEE_BNE_UN:
		return MONO_NE_RELATION;
	default:
		return MONO_ANY_RELATION;
	}
}


/*
 * Add a empty known fact to the evaluation area.
 * area: the evaluation area
 * bb_index: the identification number of the basic block related to the new known fact.
 * variable_index: the variable index related to the new known fact.
 */
static MonoKnownFact*
add_new_knownfact_to_evaluation_area (MonoKnownFactsEvaluationArea *area, int bb_index, int variable_index) {
	MonoKnownFact *base;
	MonoKnownFact *last = area->known_facts [variable_index];

	if (TRACE_RC_REMOVAL) {	
		printf("Adding empty known fact for the variable %d", variable_index);
		if (bb_index > 0)
			printf (" for the BB%d", bb_index);
		printf("\n");
	}
	
	base = (MonoKnownFact *) mono_mempool_alloc0 (area->mempool, sizeof (MonoKnownFact));

	base->next = NULL;

	if (area->known_facts [variable_index] != NULL) {
		while (last->next != NULL) {
			last = last->next;
		}

		last->next = base;
		base->prev = last;
	} else {
		base->prev = NULL;
		area->known_facts [variable_index] = base;
	}

	if (bb_index > 0) {
		base->next_known_fact_in_BB = area->bb_known_facts [bb_index];
		area->bb_known_facts [bb_index] = base;
	}

	return (base);
}

/* 
 * Remove the given known fact from the evaluation area.
 * area:  the current evaluation area 
 * known_fact: the known fact to remove
 */
static void
remove_knownfact_from_evaluation_area (MonoKnownFactsEvaluationArea *area, MonoKnownFact *known_fact){

	if (TRACE_RC_REMOVAL) {	
		printf("Removing known fact for the variable %d\n", known_fact->variable_index);
	}

	if (known_fact->prev == NULL) {
		if (known_fact->next != NULL) {
			known_fact->next->prev = NULL;
			area->known_facts [known_fact->variable_index] = known_fact->next;
		}
	} else {
		known_fact->prev->next = known_fact->next;
		if (known_fact->next != NULL) 
			known_fact->next->prev = known_fact->prev;
	}
	
	known_fact->next = NULL;
	known_fact->prev = NULL;
	
}


static void
clean_contexts (MonoKnownFactsEvaluationContext *contexts, int number) {
	int i;
	for (i = 0; i < number; i++) {
		contexts [i].status = MONO_KNOWNFACTS_EVALUATION_NOT_STARTED;
	}
}


/*
 * Perform the intersection of a range and a constant value (taking into
 * account the relation that the value has with the range).
 * range: the range that will be intersected with the value
 * value: the value that will be intersected with the range
 * relation: the relation between the range and the value
 */
static void
intersect_value( MonoKnownFactsEvaluationRange *range, int value, MonoValueRelation relation ) {
	
	switch (relation) {
	case MONO_NO_RELATION:
		MONO_MAKE_RELATIONS_EVALUATION_RANGE_IMPOSSIBLE (*range);
		break;
	case MONO_ANY_RELATION:
		break;
	case MONO_EQ_RELATION:
		MONO_UPPER_EVALUATION_RANGE_INTERSECTION (range->upper, value);
		MONO_LOWER_EVALUATION_RANGE_INTERSECTION (range->lower, value);
		break;
	case MONO_NE_RELATION: {
		/* IMPROVEMENT Figure this out! (ignoring it is safe anyway) */
		break;
	}
	case MONO_LT_RELATION:
		MONO_UPPER_EVALUATION_RANGE_INTERSECTION (range->upper, MONO_UPPER_EVALUATION_RANGE_NOT_EQUAL (value));
		break;
	case MONO_LE_RELATION:
		MONO_UPPER_EVALUATION_RANGE_INTERSECTION (range->upper, value);
		break;
	case MONO_GT_RELATION:
		MONO_LOWER_EVALUATION_RANGE_INTERSECTION (range->lower, MONO_LOWER_EVALUATION_RANGE_NOT_EQUAL (value));
		break;
	case MONO_GE_RELATION:
		MONO_LOWER_EVALUATION_RANGE_INTERSECTION (range->lower, value);
		break;
	default:
		g_assert_not_reached();
	}
}


/*
 * Perform the intersection of two pairs of ranges (taking into account the
 * relation between the ranges and a given delta).
 * ranges: the ranges that will be intersected
 * other_ranges the other ranges that will be intersected
 * delta: the delta between the pairs of ranges
 * relation: the relation between the pairs of ranges
 */
static void
intersect_ranges( MonoKnownFactsEvaluationRanges *ranges, MonoKnownFactsEvaluationRanges *other_ranges, int delta, MonoValueRelation relation ) {
	
	if (delta == 0) {
		switch (relation) {
		case MONO_NO_RELATION:
			MONO_MAKE_RELATIONS_EVALUATION_RANGES_IMPOSSIBLE (*ranges);
			break;
		case MONO_ANY_RELATION:
			break;
		case MONO_EQ_RELATION:			
			MONO_RELATIONS_EVALUATION_RANGES_INTERSECTION (*ranges, *other_ranges);
			break;
		case MONO_NE_RELATION: {
			/* FIXME Figure this out! (ignoring it is safe anyway) */
			break;
		}
		case MONO_LT_RELATION:
			MONO_UPPER_EVALUATION_RANGE_INTERSECTION (ranges->zero.upper, MONO_UPPER_EVALUATION_RANGE_NOT_EQUAL (other_ranges->zero.upper));
			MONO_UPPER_EVALUATION_RANGE_INTERSECTION (ranges->variable.upper, MONO_UPPER_EVALUATION_RANGE_NOT_EQUAL (other_ranges->variable.upper));
			break;
		case MONO_LE_RELATION:
			MONO_UPPER_EVALUATION_RANGE_INTERSECTION (ranges->zero.upper, other_ranges->zero.upper);
			MONO_UPPER_EVALUATION_RANGE_INTERSECTION (ranges->variable.upper, other_ranges->variable.upper);
			break;
		case MONO_GT_RELATION:
			MONO_LOWER_EVALUATION_RANGE_INTERSECTION (ranges->zero.lower, MONO_LOWER_EVALUATION_RANGE_NOT_EQUAL (other_ranges->zero.lower));
			MONO_LOWER_EVALUATION_RANGE_INTERSECTION (ranges->variable.lower, MONO_LOWER_EVALUATION_RANGE_NOT_EQUAL (other_ranges->variable.lower));
			break;
		case MONO_GE_RELATION:
			MONO_LOWER_EVALUATION_RANGE_INTERSECTION (ranges->zero.lower, other_ranges->zero.lower);
			MONO_LOWER_EVALUATION_RANGE_INTERSECTION (ranges->variable.lower, other_ranges->variable.lower);
			break;
		default:
			g_assert_not_reached();
		}
	} else {
		MonoKnownFactsEvaluationRanges translated_ranges = *other_ranges;
		MONO_ADD_DELTA_SAFELY_TO_RANGES (translated_ranges, delta);
		intersect_ranges( ranges, &translated_ranges, FALSE, relation );
	}
}

/*
 * Recursive method that traverses the relation graph to evaluate the
 * relation between two variables.
 * At the end of the execution, the resulting ranges are in the context of
 * the "starting" variable.
 * area: the current evaluation area (it contains the relation graph and
 *       memory for all the evaluation contexts is already allocated)
 * variable: starting variable (the value ranges in its context are the result
 *           of the execution of this procedure)
 * target_variable: the variable with respect to which the starting variable
 *                  is evaluated (tipically the starting variable is the index
 *                  and the target one is the array (which means its length))
 * father_context: the previous evaluation context in recursive invocations
 *                 (or NULL for the first invocation)
 */
static void
evaluate_relation_with_target_variable (MonoKnownFactsEvaluationArea *area, int variable, int target_variable, MonoKnownFactsEvaluationContext *father_context) {
	MonoKnownFactsEvaluationContext *context = &(area->contexts [variable]);

	// First of all, we check the evaluation status
	// (what must be done is *very* different in each case)
	switch (context->status) {
	case MONO_KNOWNFACTS_EVALUATION_NOT_STARTED: {
		MonoKnownFact *relation = area->known_facts [variable];
		
		if (TRACE_RC_REMOVAL) {
			if (relation) {
				printf ("Evaluating variable %d (target variable %d)\n", variable, target_variable);
				print_summarized_value_relation (relation);
				printf ("\n");
			} else {
				printf ("Variable %d haven't any known fact\n", variable);
			}
		}

		// We properly inizialize the context
		context->status = MONO_KNOWNFACTS_EVALUATION_IN_PROGRESS;
		context->father = father_context;
		MONO_MAKE_RELATIONS_EVALUATION_RANGES_WEAK (context->ranges);
		
		// If we have found the target variable, we can set the range
		// related to it in the context to "equal" (which is [0,0])
		if (variable == target_variable) {
			if (TRACE_RC_REMOVAL) {
				printf ("Target variable reached (%d), continuing to evaluate relations with constants\n", variable);
			}
			context->ranges.variable.lower = 0;
			context->ranges.variable.upper = 0;
		}
		
		// Examine all relations for this variable (scan the list)
		// The contribute of each relation will be intersected (logical and)
		while (relation != NULL) {
			context->current_relation = relation;
			
			if (TRACE_RC_REMOVAL) {
				printf ("Processing (%d): ", variable);
				print_summarized_value_relation (relation);
				printf ("\n");
			}
			
			// We decie what to do according the the type of the related value
			switch (relation->type) {
			case MONO_EMPTY_RELATION:
				// No added information, skip it
				break;
			case MONO_CONSTANT_RELATION:
				// Intersect range with constant (taking into account the relation)
				intersect_value (&(context->ranges.zero), relation->value.constant.value, relation->value.constant.relation);
				break;
			case MONO_VARIABLE_RELATION:
				// Generally, evaluate related variable and intersect ranges.
				// However, some check is necessary...
				
				// If the relation is "ANY", nothing to do (no added information)
				if (relation->value.variable.relation != MONO_ANY_RELATION) {
					int related_variable = relation->value.variable.variable;
					MonoKnownFactsEvaluationContext *related_context = &(area->contexts [related_variable]);
					
					// The second condition in the "or" avoids messing with "back edges" in the graph traversal
					// (they are simply ignored instead of triggering the handling of recursion)
					if ( (related_context->status == MONO_KNOWNFACTS_EVALUATION_NOT_STARTED) || !
							((related_context->current_relation->type == MONO_VARIABLE_RELATION) &&
							(related_context->current_relation->value.variable.variable == variable))) {
						// Evaluate the related variable
						evaluate_relation_with_target_variable (area, related_variable, target_variable, context);
						
						// Check if we are part of a recursive loop
						if (context->status & MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVE) {
							if (TRACE_RC_REMOVAL) {
								printf ("Recursivity detected for varible %d (target variable %d), status ", variable, target_variable);
								print_evaluation_context_status (context->status);
							}
							//printf ("RCRRecursivity detected for varible %d (target variable %d), status ", variable, target_variable);
							
							// If we are, check if the evaluation of the related variable is complete
							if (related_context->status == MONO_KNOWNFACTS_EVALUATION_COMPLETED) {
								// If it is complete, we are part of a recursive definition.
								// Since it is a *definition* (and definitions are evaluated *before*
								// conditions because they are first in the list), intersection is not
								// strictly necessary, we simply copy the ranges and apply the delta
								context->ranges = related_context->ranges;
								/* Delta has already been checked for over/under-flow when evaluating values */
								MONO_ADD_DELTA_SAFELY_TO_RANGES (context->ranges, relation->value.variable.delta);
								context->status = MONO_KNOWNFACTS_EVALUATION_COMPLETED;
								if (TRACE_RC_REMOVAL) {
									printf (", ranges already computed, result: \n");
									print_evaluation_context_ranges (&(context->ranges));
									printf (" (delta is %d)\n", relation->value.variable.delta);
								}
							} else {
								// If it is not complete, do nothing (we do not have enough information)
								if (TRACE_RC_REMOVAL) {
									printf (", ranges not computed\n");
								}
							}
						} else {
							// If we are not (the common case) intersect the result
							intersect_ranges( &(context->ranges), &(related_context->ranges), relation->value.variable.delta, relation->value.variable.relation );
						}
					} else {
						if (TRACE_RC_REMOVAL) {
							printf ("Relation is a back-edge in this traversal, skipping\n");
						}
					}
				}
				break;
			case MONO_PHI_RELATION: {
				// We must compute all PHI alternatives, combining the results (with a union, which is a logical "or"),
				// and intersect this result with the ranges in the context; we must also take into account recursions
				// (with loops that can be ascending, descending, or indefinite)
				MonoKnownFactsEvaluationRanges phi_ranges;
				int phi;
				gboolean is_ascending = FALSE;
				gboolean is_descending = FALSE;
				
				MONO_MAKE_RELATIONS_EVALUATION_RANGES_IMPOSSIBLE (phi_ranges);
				for (phi = 0; phi < relation->value.phi.number_of_alternatives; phi++) {
					int phi_alternative = relation->value.phi.phi_alternatives [phi];
					evaluate_relation_with_target_variable (area, phi_alternative, target_variable, context);
					
					// This means we are part of a recursive loop
					if (context->status & MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVE) {
						if (TRACE_RC_REMOVAL) {
							printf ("Recursivity detected for variable %d (target variable %d), status ", variable, target_variable);
							print_evaluation_context_status (context->status);
							printf ("\n");
						}
						if (context->status & MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_ASCENDING) {
							is_ascending = TRUE;
						}
						if (context->status & MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_DESCENDING) {
							is_descending = TRUE;
						}
						if (context->status & MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_INDEFINITE) {
							is_ascending = TRUE;
							is_descending = TRUE;
						}
						
						// Clear "recursivity" bits in the status (recursion has been handled)
						context->status = MONO_KNOWNFACTS_EVALUATION_IN_PROGRESS;
					} else {
						MONO_RELATIONS_EVALUATION_RANGES_UNION (phi_ranges, area->contexts [phi_alternative].ranges);
					}
				}
				
				// Apply the effects of all recursive loops
				if (is_ascending) {
					phi_ranges.zero.upper = INT_MAX;
					phi_ranges.variable.upper = INT_MAX;
				}
				if (is_descending) {
					phi_ranges.zero.lower = INT_MIN;
					phi_ranges.variable.lower = INT_MIN;
				}
				
				// Intersect final result
				MONO_RELATIONS_EVALUATION_RANGES_INTERSECTION (context->ranges, phi_ranges);
				break;
			}
			case MONO_CLASS_RELATION:
				if (TRACE_RC_REMOVAL) {
					printf ("Class relation for variable: %d\n", variable);
					}
				break;
			default:
				g_assert_not_reached();
			}
			
			// Pass to next relation
			relation = relation->next;
		}
		
		// Check if any recursivity bits are still in the status, and in any case clear them
		if (context->status & MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVE) {
			if (TRACE_RC_REMOVAL) {
				printf ("Recursivity for varible %d (target variable %d) discards computation, status ", variable, target_variable);
				print_evaluation_context_status (context->status);
				printf ("\n");
			}
			// If yes, we did not have enough information (most likely we were evaluated inside a PHI, but we also
			// depended on the same PHI, which was still in evaluation...), so clear the status to "NOT_STARTED"
			// (if we will be evaluated again, the PHI will be already done, so our evaluation will succeed)
			context->status = MONO_KNOWNFACTS_EVALUATION_NOT_STARTED;
		} else {
			if (TRACE_RC_REMOVAL) {
				printf ("Ranges for varible %d (target variable %d) computed: ", variable, target_variable);
				print_evaluation_context_ranges (&(context->ranges));
				printf ("\n");
			}
			// If not (the common case) the evaluation is complete, and the result is in the context
			context->status = MONO_KNOWNFACTS_EVALUATION_COMPLETED;
		}
		break;
	}
	case MONO_KNOWNFACTS_EVALUATION_IN_PROGRESS: {
		// This means we are in a recursive loop
		MonoKnownFactsEvaluationContext *current_context = father_context;
		MonoKnownFactsEvaluationContext *last_context = context->father;
		gboolean evaluation_can_be_recursive = TRUE;
		gboolean evaluation_is_definition = TRUE;
		int path_value = 0;
		
		if (TRACE_RC_REMOVAL) {
			printf ("Evaluation of variable %d (target variable %d) already in progress\n", variable, target_variable);
			print_evaluation_context (context);
			print_summarized_value_relation (context->current_relation);
			printf ("\n");
		}
		
		// We must check if the loop can be a recursive definition (we scan the whole loop)
		while (current_context != last_context) {
			if (current_context == NULL) {
				printf ("Broken recursive ring in ABC removal\n");
				g_assert_not_reached ();
			}
			
			if (current_context->current_relation->relation_is_static_definition) {
				if (current_context->current_relation->type == MONO_VARIABLE_RELATION) {
					/* No need to check path_value for over/under-flow, since delta should be safe */
					path_value += current_context->current_relation->value.variable.delta;
				} else if (current_context->current_relation->type != MONO_PHI_RELATION) {
					evaluation_can_be_recursive = FALSE;
				}
			} else {
				evaluation_is_definition = FALSE;
				evaluation_can_be_recursive = FALSE;
			}
			
			current_context = current_context->father;
		}
		
		// If this is a recursive definition, we properly flag the status in all the involved contexts
		if (evaluation_is_definition) {
			MonoKnownFactsEvaluationStatus recursive_status;
			if (evaluation_can_be_recursive) {
				if (path_value > 0) {
					recursive_status = MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_ASCENDING;
				} else if (path_value < 0) {
					recursive_status = MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_DESCENDING;
				} else {
					recursive_status = MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_INDEFINITE;
				}
			} else {
				recursive_status = MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_INDEFINITE;
			}
			
			if (TRACE_RC_REMOVAL) {
				printf ("Recursivity accepted (");
				print_evaluation_context_status (recursive_status);
				printf (")\n");
			}
			
			current_context = father_context;
			while (current_context != last_context) {
				current_context->status |= recursive_status;
				current_context = current_context->father;
			}
		} else {
			if (TRACE_RC_REMOVAL) {
				printf ("Recursivity rejected (some relation in the cycle is not a defintion)\n");
			}
		}
		break;
	}
	case MONO_KNOWNFACTS_EVALUATION_COMPLETED: {
		return;
	}
	default:
		if (TRACE_RC_REMOVAL) {
			printf ("Variable %d (target variable %d) already in a recursive ring, skipping\n", variable, target_variable);
			print_evaluation_context (context);
			print_summarized_value_relation (context->current_relation);
			printf ("\n");
		}
		break;
	}
	
}


/*
 * Apply the given value kind to the given range
 */
static void apply_value_kind_to_range (MonoKnownFactsEvaluationRange *range, MonoIntegerValueKind value_kind) {
	
	
	if (value_kind != MONO_UNKNOWN_INTEGER_VALUE) {
		if (value_kind & MONO_UNSIGNED_VALUE_FLAG) {
			if (range->lower < 0) {
				range->lower = 0;
			}
			if ((value_kind & MONO_INTEGER_VALUE_SIZE_BITMASK) == 1) {
				if (range->upper > 0xff) {
					range->upper = 0xff;
				}
			} else if ((value_kind & MONO_INTEGER_VALUE_SIZE_BITMASK) == 2) {
				if (range->upper > 0xffff) {
					range->upper = 0xffff;
				}
			}
		} else {
			if ((value_kind & MONO_INTEGER_VALUE_SIZE_BITMASK) == 1) {
				if (range->lower < -0x80) {
					range->lower = -0x80;
				}
				if (range->upper > 0x7f) {
					range->upper = 0x7f;
				}
			} else if ((value_kind & MONO_INTEGER_VALUE_SIZE_BITMASK) == 2) {
				if (range->lower < -0x8000) {
					range->lower = -0x8000;
				}
				if (range->upper > 0x7fff) {
					range->upper = 0x7fff;
				}
			}
		}
	}
}



/*
 * Recursively scan a tree of MonoInst for add new "known facts" and look for checks removal.
 * inst: the root of the MonoInst tree
 * area: the current evaluation area (it contains the relation graph and
 *       memory for all the evaluation contexts is already allocated)
 * result: 
 * bb_scope: 
 * value_context:
 *
 * Optional arguments:
 * in MONO_ARRAY_CONTEXT
 *    is_array_type: 
 * in MONO_INTEGER_CONTEXT
 *    result_value_kind: 
 *    p_value_kind: 
 * in MONO_CONDITIONALBRANCH_CONTEXT
 *    branch_relation:
 */
static void
process_inst (MonoInst *inst, MonoBasicBlock *bb, MonoKnownFactsEvaluationArea *area,
			  MonoKnownFact *result, gboolean bb_scope, MonoValueContext value_context, ...)
{	
	MonoKnownFact *known_fact;
	MonoKnownFact left_value;
	MonoKnownFact right_value;
	va_list ap;
	gboolean is_array_type;
	MonoIntegerValueKind result_value_kind;
	MonoIntegerValueKind value_kind;
	MonoIntegerValueKind *p_value_kind;
	MonoValueRelation branch_relation;
	int variable_index;
	MonoKnownFactsEvaluationRange range;
	
	if (TRACE_RC_REMOVAL) {
		printf ("Processing instruction: ");
		mono_print_tree (inst);
		if (bb_scope)
			printf (" in BB%d.", bb->block_num);
		printf ("\n");
	}
	
	va_start (ap, value_context);
	
	switch (value_context) {
	case MONO_ARRAY_CONTEXT:
		is_array_type = va_arg(ap, gboolean);
		break;
	case MONO_INTEGER_CONTEXT:
	    result_value_kind = va_arg (ap, MonoIntegerValueKind);
	    p_value_kind = va_arg (ap, MonoIntegerValueKind*);
	    break;
	case MONO_CONDITIONALBRANCH_CONTEXT:
		branch_relation = va_arg (ap, MonoValueRelation);
	default:
		break;
	}
	va_end(ap);
	
	
	if (value_context == MONO_INTEGER_CONTEXT) {
		if (inst->type == STACK_I8) {
			*p_value_kind = MONO_INTEGER_VALUE_SIZE_8;
		} else if (inst->type == STACK_I4) {
			*p_value_kind = MONO_INTEGER_VALUE_SIZE_4;
		} else {
			*p_value_kind = MONO_UNKNOWN_INTEGER_VALUE;
		}
	}
	
	switch (inst->opcode) {
	case CEE_LDELEMA: {
		if (TRACE_RC_REMOVAL) {
			printf ("Attempting ABC removal...\n");
		}
		
		MonoInst *array_inst = inst->inst_left;
		MonoInst *index_inst = inst->inst_right;
		MonoIntegerValueKind index_value_kind;
		MonoKnownFact array_value;
		MonoKnownFact index_value;
		
		process_inst (array_inst, bb, area, &array_value, bb_scope, MONO_ARRAY_CONTEXT, TRUE);
		process_inst (index_inst, bb, area, &index_value, bb_scope, MONO_INTEGER_CONTEXT, MONO_UNKNOWN_INTEGER_VALUE, &value_kind);
		
		if (array_value.type == MONO_VARIABLE_RELATION) {
			int array_variable = array_value.value.variable.variable;
			MonoKnownFactsEvaluationContext *array_context = &(area->contexts [array_variable]);
			
			if (index_value.type == MONO_CONSTANT_RELATION) {
				// The easiest case: we just evaluate the array length, to see if it has some relation
				// with the index constant, and act accordingly
				
				clean_contexts (area->contexts, area->cfg->num_varinfo);
				evaluate_relation_with_target_variable (area, array_variable, array_variable, NULL);
				
				if ((index_value.value.constant.value >= 0) && (index_value.value.constant.value < array_context->ranges.zero.lower)) {
					if (REPORT_RC_REMOVAL) {
						printf ("ARRAY-ACCESS: removed bounds check on array %d with constant index %d in method %s\n", array_variable, index_value.value.constant.value, mono_method_full_name (area->cfg->method, TRUE));
					}
					inst->flags |= (MONO_INST_NORANGECHECK);
				}
			} else if (index_value.type == MONO_VARIABLE_RELATION) {
				// The common case: we must evaluate both the index and the array length, and check for relevant
				// relations both through variable definitions and as constant definitions
				
				int index_variable = index_value.value.variable.variable;
				MonoKnownFactsEvaluationContext *index_context = &(area->contexts [index_variable]);
				
				clean_contexts (area->contexts, area->cfg->num_varinfo);
				
				evaluate_relation_with_target_variable (area, index_variable, array_variable, NULL);
				evaluate_relation_with_target_variable (area, array_variable, array_variable, NULL);

				MONO_SUB_DELTA_SAFELY_FROM_RANGES (index_context->ranges, index_value.value.variable.delta);
				apply_value_kind_to_range (&(index_context->ranges.zero), index_value_kind);

				if (index_context->ranges.zero.lower >= 0) {
					if (TRACE_RC_REMOVAL) {
						printf ("ARRAY-ACCESS: Removed lower bound check on array %d with index %d\n", array_variable, index_variable);
					}
					if ((index_context->ranges.variable.upper < 0)||(index_context->ranges.zero.upper < array_context->ranges.zero.lower)) {
						if (REPORT_RC_REMOVAL) {
							printf ("ARRAY-ACCESS: removed bounds check on array %d with index %d in method %s\n", array_variable, index_variable, mono_method_full_name (area->cfg->method, TRUE));
						}				
						inst->flags |= (MONO_INST_NORANGECHECK);
					}
				}		
				
				if (TRACE_RC_REMOVAL) {
				    if (index_context->ranges.variable.upper < 0) {
						printf ("ARRAY-ACCESS: Removed upper bound check (through variable) on array %d with index %d\n", array_variable, index_variable);
					}
				    if (index_context->ranges.zero.upper < array_context->ranges.zero.lower) {
						printf ("ARRAY-ACCESS: Removed upper bound check (through constant) on array %d with index %d\n", array_variable, index_variable);
					}
				}
			}
		} else if (array_value.type == MONO_CONSTANT_RELATION) {
			if (index_value.type == MONO_CONSTANT_RELATION) {
				if ((index_value.value.constant.value >= 0) && (index_value.value.constant.value < array_value.value.constant.value)) {
					if (REPORT_RC_REMOVAL) {
						printf ("ARRAY-ACCESS: removed bounds check on array of constant length %d with constant index %d in method %s\n", array_value.value.constant.value, index_value.value.constant.value, mono_method_full_name (area->cfg->method, TRUE));
					}
					inst->flags |= (MONO_INST_NORANGECHECK);
				}
			} else if (index_value.type == MONO_VARIABLE_RELATION) {
				int index_variable = index_value.value.variable.variable;
				MonoKnownFactsEvaluationContext *index_context = &(area->contexts [index_variable]);
				
				clean_contexts (area->contexts, area->cfg->num_varinfo);
				evaluate_relation_with_target_variable (area, index_variable, index_variable, NULL);
				apply_value_kind_to_range (&(index_context->ranges.zero), index_value_kind);
				
				if ((index_context->ranges.zero.lower >= 0) && (index_context->ranges.zero.upper < array_value.value.constant.value)) {
					if (REPORT_RC_REMOVAL) {	
						printf ("ARRAY-ACCESS: removed bounds check on array of constant length %d with index %d ranging from %d to %d in method %s\n", array_value.value.constant.value, index_variable, index_context->ranges.zero.lower, index_context->ranges.zero.upper, mono_method_full_name (area->cfg->method, TRUE));
					}
					inst->flags |= (MONO_INST_NORANGECHECK);
				}
			} else if (index_value_kind != MONO_UNKNOWN_INTEGER_VALUE) {
				MonoKnownFactsEvaluationRange range;
				MONO_MAKE_RELATIONS_EVALUATION_RANGE_WEAK (range);
				apply_value_kind_to_range (&range, index_value_kind);
				
				if ((range.lower >= 0) && (range.upper < array_value.value.constant.value)) {
					if (REPORT_RC_REMOVAL) {
						printf ("ARRAY-ACCESS: removed bounds check on array of constant length %d with unknown index ranging from %d to %d in method %s\n", array_value.value.constant.value, range.lower, range.upper, mono_method_full_name (area->cfg->method, TRUE));
					}
					inst->flags |= (MONO_INST_NORANGECHECK);
				}
			}
		}
		MAKE_VALUE_ANY (*result);
		break;
	}
	case CEE_STIND_REF: {
		if ((inst->inst_left->opcode == OP_LOCAL) || (inst->inst_left->opcode == OP_ARG)) {
			variable_index = inst->inst_left->inst_c0;
			known_fact = add_new_knownfact_to_evaluation_area (area, -1, variable_index);
			known_fact->type = MONO_CLASS_RELATION;
			known_fact->value.klass.relation = MONO_IS_ISTANCE_OF;
			known_fact->value.klass.klass = inst->inst_left->klass;
			known_fact->relation_is_static_definition = TRUE;
			known_fact->variable_index = variable_index;
			
			if (TRACE_RC_REMOVAL) {
				printf ("Adding relation on variable %d: ", variable_index);
				print_summarized_value_relation (known_fact);
				printf ("\n");
			}
		}
	}
	case CEE_STIND_I:
	case CEE_STIND_I4:
	case CEE_STIND_I1:
	case CEE_STIND_I2:
	case CEE_STIND_I8:
	case CEE_STIND_R4:
	case CEE_STIND_R8: {
		if (value_context == MONO_ROOT_CONTEXT){
			if (TRACE_RC_REMOVAL) {
				printf ("[store instruction found]\n");
			}
			if ((inst->inst_left->opcode == OP_LOCAL) || (inst->inst_left->opcode == OP_ARG)) {
				if (!bb_scope) {
					variable_index = inst->inst_left->inst_c0;

				    switch (inst->inst_left->inst_vtype->type) {
					case MONO_TYPE_ARRAY:
					case MONO_TYPE_SZARRAY:
						is_array_type = TRUE;
					    goto handle_array_value;
					case MONO_TYPE_OBJECT:
						is_array_type = FALSE;
handle_array_value:
					    process_inst(inst->inst_right, bb, area, result, bb_scope, MONO_ARRAY_CONTEXT, is_array_type);
					    break;
					case MONO_TYPE_I1:
						area->variable_value_kind [variable_index] = MONO_INTEGER_VALUE_SIZE_1;
					    goto handle_integer_value;
					case MONO_TYPE_U1:
						area->variable_value_kind [variable_index] = MONO_UNSIGNED_INTEGER_VALUE_SIZE_1;
					    goto handle_integer_value;
					case MONO_TYPE_I2:
						area->variable_value_kind [variable_index] = MONO_INTEGER_VALUE_SIZE_2;
					    goto handle_integer_value;
					case MONO_TYPE_U2:
						area->variable_value_kind [variable_index] = MONO_UNSIGNED_INTEGER_VALUE_SIZE_2;
					    goto handle_integer_value;
					case MONO_TYPE_I4:
						area->variable_value_kind [variable_index] = MONO_INTEGER_VALUE_SIZE_4;
					    goto handle_integer_value;
					case MONO_TYPE_U4:
						area->variable_value_kind [variable_index] = MONO_UNSIGNED_INTEGER_VALUE_SIZE_4;
					    goto handle_integer_value;
					case MONO_TYPE_I:
						area->variable_value_kind [variable_index] = SIZEOF_VOID_P;
					    goto handle_integer_value;
					case MONO_TYPE_U:
					    area->variable_value_kind [variable_index] = (MONO_UNSIGNED_VALUE_FLAG|SIZEOF_VOID_P);
					    goto handle_integer_value;
					case MONO_TYPE_I8:
						area->variable_value_kind [variable_index] = MONO_INTEGER_VALUE_SIZE_8;
					    goto handle_integer_value;
					case MONO_TYPE_U8:
						area->variable_value_kind [variable_index] = MONO_UNSIGNED_INTEGER_VALUE_SIZE_8;
handle_integer_value:										
					    process_inst(inst->inst_right, bb, area, result, bb_scope, MONO_INTEGER_CONTEXT, area->variable_value_kind [variable_index], &value_kind);
						MONO_MAKE_RELATIONS_EVALUATION_RANGE_WEAK (range);
					    apply_value_kind_to_range (&range, area->variable_value_kind [variable_index]);
					    apply_value_kind_to_range (&range, value_kind);					
					    break;
					default:
						MAKE_VALUE_ANY (*result);
				    }
				
					if (value_context == MONO_INTEGER_CONTEXT) {			
						if (range.upper < INT_MAX) {
							known_fact = add_new_knownfact_to_evaluation_area (area, -1, variable_index);
							known_fact->type = MONO_CONSTANT_RELATION;
							known_fact->value.constant.relation = MONO_LE_RELATION;					
							known_fact->value.constant.value = range.upper;
							known_fact->relation_is_static_definition = TRUE;
							if (TRACE_RC_REMOVAL) {
								printf ("Adding relation on variable %d (relation_type: %d) (inst_type: %d): ", variable_index, known_fact->type, inst->inst_left->inst_vtype->type);
								print_summarized_value_relation (known_fact);
								printf (" [var%d <= %d]", variable_index, range.upper);
								printf ("\n");
							}
						}
					    if (range.lower > INT_MIN) {
							known_fact = add_new_knownfact_to_evaluation_area (area, -1, variable_index);
							known_fact->type = MONO_CONSTANT_RELATION;							
							known_fact->value.constant.relation = MONO_GE_RELATION;					
							known_fact->value.constant.value = range.lower;
							known_fact->relation_is_static_definition = TRUE;
							if (TRACE_RC_REMOVAL) {
								printf ("Adding relation on variable %d (relation_type: %d) (inst_type: %d): ", variable_index, known_fact->type, inst->inst_left->inst_vtype->type);
								print_summarized_value_relation (known_fact);
								printf (" [var%d >= %d]", variable_index, range.lower);
								printf ("\n");	
							}
						}
						break;
					}
					
				    if (result->type != MONO_EMPTY_RELATION) {
						known_fact = add_new_knownfact_to_evaluation_area (area, -1, variable_index);
					
						known_fact->variable_index = variable_index;
						known_fact->type = result->type;
						known_fact->relation_is_static_definition = TRUE;
					    if (result->type == MONO_CONSTANT_RELATION) {
							known_fact->value.constant.relation = result->value.constant.relation;
							known_fact->value.constant.value = result->value.constant.value;
						} else if (result->type == MONO_VARIABLE_RELATION) {
							known_fact->value.variable.variable = result->value.variable.variable;
							known_fact->value.variable.relation = result->value.variable.relation;
							known_fact->value.variable.delta = result->value.variable.delta;
						} else if (result->type == MONO_PHI_RELATION) {
							known_fact->value.phi.number_of_alternatives = result->value.phi.number_of_alternatives;
							known_fact->value.phi.relation = result->value.phi.relation;
							known_fact->value.phi.phi_alternatives = result->value.phi.phi_alternatives;
						}
					
					    if (TRACE_RC_REMOVAL) {
							printf ("Adding relation on variable %d: ", variable_index);
							print_summarized_value_relation (known_fact);
							printf ("\n");						
						}
										
					    if (result->type == MONO_VARIABLE_RELATION) {
							int related_variable = known_fact->value.variable.variable;
							known_fact = add_new_knownfact_to_evaluation_area (area, -1, related_variable);
							known_fact->value.variable.relation = MONO_EQ_RELATION;
							known_fact->relation_is_static_definition = TRUE;
							known_fact->type = MONO_VARIABLE_RELATION;
							known_fact->value.variable.variable = variable_index;
							known_fact->value.variable.delta = - area->known_facts [variable_index]->value.variable.delta;
							if (TRACE_RC_REMOVAL) {
								printf ("Added symmetric relation on variable %d (with %d): ", related_variable, variable_index);
								print_summarized_value_relation (known_fact);
								printf ("\n");
							}
						}	
					} else {
						if (TRACE_RC_REMOVAL) {
							printf ("Variable %d not handled (instruction type: %x)\n", variable_index, inst->inst_vtype->type);
						}
					}
			    }
			} else  {
				MONO_INST_ARITY (inst);
				/*
				if (result->type != MONO_EMPTY_RELATION) {
				    if ( bb_scope ) 
						known_fact = add_new_knownfact_to_evaluation_area (area, bb->block_num, variable_index);
				    else 
				        known_fact = add_new_knownfact_to_evaluation_area (area, -1, variable_index);
				    
				}
				*/
			}
		} else {
			MAKE_VALUE_ANY (*result); /*Useless?*/
		}
		break;	
	}
	case OP_LOCAL:
	case OP_ARG:
		result->type = MONO_VARIABLE_RELATION;
		result->value.variable.variable = inst->inst_c0;
		result->value.variable.delta = 0;
	    result->value.variable.relation = MONO_EQ_RELATION;
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = area->variable_value_kind [inst->inst_c0];

		break;
	case CEE_LDIND_REF:
		if (value_context == MONO_ARRAY_CONTEXT){
			process_inst (inst->inst_left, bb, area, result, bb_scope, MONO_ARRAY_CONTEXT, FALSE);
		} else {
			MAKE_VALUE_ANY (*result);
		}
		break;
	case CEE_NEWARR:
		if (value_context == MONO_ARRAY_CONTEXT) {
			process_inst (inst->inst_newa_len, bb, area, result, bb_scope, MONO_INTEGER_CONTEXT, MONO_UNKNOWN_INTEGER_VALUE, &value_kind);
		} else {
			MAKE_VALUE_ANY (*result);
		}
		break;
		
#if SIZEOF_VOID_P == 8
	case OP_I8CONST: {
		if (value_context == MONO_ARRAY_CONTEXT) {
			if ((is_array_type) && (inst->inst_p0 != NULL)) {
				MonoArray *array = (MonoArray *) (inst->inst_p0);
				result->type = MONO_CONSTANT_RELATION;
				result->value.constant.value = array->max_length;
				result->value.constant.relation = MONO_EQ_RELATION;
			} else {
				MAKE_VALUE_ANY (*result);
			}
		} else {
				MAKE_VALUE_ANY (*result);
		}
		break;
	}
	case OP_ICONST:
		if (value_context == MONO_INTEGER_CONTEXT) {
			result->type = MONO_CONSTANT_RELATION;
			result->value.constant.value = inst->inst_c0;
			result->value.constant.relation = MONO_EQ_RELATION;
		} else {
			MAKE_VALUE_ANY (*result);
		}
		break;
#else
	case OP_ICONST: {
		if (value_context == MONO_ARRAY_CONTEXT) {
			if ((is_array_type) && (inst->inst_p0 != NULL)) {
				MonoArray *array = (MonoArray *) (inst->inst_p0);
				result->type = MONO_CONSTANT_RELATION;
				result->value.constant.value = array->max_length;
				result->value.constant.relation = MONO_EQ_RELATION;
			} else {
				MAKE_VALUE_ANY (*result);
			}
		} else if (value_context == MONO_INTEGER_CONTEXT) {
			result->type = MONO_CONSTANT_RELATION;
			result->value.constant.value = inst->inst_c0;
			result->value.constant.relation = MONO_EQ_RELATION;
		} else {
				MAKE_VALUE_ANY (*result);
		}
		break;
	}
#endif
	case OP_PHI:
		result->type = MONO_PHI_RELATION;
		result->value.phi.relation = MONO_EQ_RELATION;
		result->value.phi.number_of_alternatives = *(inst->inst_phi_args);
		result->value.phi.phi_alternatives = inst->inst_phi_args + 1;
		break;
	case CEE_LDIND_I1:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_INTEGER_VALUE_SIZE_1;
		goto handle_load;
	case CEE_LDIND_U1:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_UNSIGNED_INTEGER_VALUE_SIZE_1;
		goto handle_load;
	case CEE_LDIND_I2:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_INTEGER_VALUE_SIZE_2;
		goto handle_load;
	case CEE_LDIND_U2:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_UNSIGNED_INTEGER_VALUE_SIZE_2;
		goto handle_load;
	case CEE_LDIND_I4:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_INTEGER_VALUE_SIZE_4;
		goto handle_load;
	case CEE_LDIND_U4:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_UNSIGNED_INTEGER_VALUE_SIZE_4;
		goto handle_load;
	case CEE_LDIND_I8:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_INTEGER_VALUE_SIZE_8;
		goto handle_load;
	case CEE_LDIND_I:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = SIZEOF_VOID_P;
handle_load:
		if ((inst->inst_left->opcode == OP_LOCAL) || (inst->inst_left->opcode == OP_ARG)) {
			process_inst (inst->inst_left, bb, area, result, bb_scope, MONO_INTEGER_CONTEXT, result_value_kind, &value_kind);
		} else {
			MONO_INST_ARITY (inst);
		}
		break;
	case CEE_ADD: {
		if (value_context == MONO_INTEGER_CONTEXT) {
			process_inst (inst->inst_left, bb, area, &left_value, bb_scope, MONO_INTEGER_CONTEXT, result_value_kind, &value_kind);
			process_inst (inst->inst_right, bb, area, &right_value, bb_scope, MONO_INTEGER_CONTEXT, result_value_kind, &value_kind);
		
		    if (left_value.type == MONO_VARIABLE_RELATION) {
			    if (right_value.type == MONO_CONSTANT_RELATION) {
					result->type = MONO_VARIABLE_RELATION;
					result->value.variable.variable = left_value.value.variable.variable;
					result->value.variable.delta = left_value.value.variable.delta + right_value.value.constant.value;
				} else {
					MAKE_VALUE_ANY (*result);
				}
			} else if (right_value.type == MONO_VARIABLE_RELATION) {
			    if (left_value.type == MONO_CONSTANT_RELATION) {
					result->type = MONO_VARIABLE_RELATION;
					result->value.variable.variable = right_value.value.variable.variable;
					result->value.variable.delta = left_value.value.constant.value + right_value.value.variable.delta;
				} else {
					MAKE_VALUE_ANY (*result);
				}
		    } else if ((right_value.type == MONO_CONSTANT_RELATION) && (left_value.type == MONO_CONSTANT_RELATION)) {
				result->type = MONO_CONSTANT_RELATION;
				result->value.constant.value = left_value.value.constant.value + right_value.value.constant.value;
			} else {
				MAKE_VALUE_ANY (*result);
			}
			
		    if (result->type == MONO_VARIABLE_RELATION) {
				check_delta_safety (area, result);
			}
			
		} else {
			MAKE_VALUE_ANY (*result);
		}
	break;
	}
	case CEE_SUB: {
		if (value_context == MONO_INTEGER_CONTEXT){
			process_inst (inst->inst_left, bb, area, &left_value, bb_scope, MONO_INTEGER_CONTEXT, result_value_kind, &value_kind);
			process_inst (inst->inst_right, bb, area, &right_value, bb_scope, MONO_INTEGER_CONTEXT, result_value_kind, &value_kind);

		    if (left_value.type == MONO_VARIABLE_RELATION) {
			    if (right_value.type == MONO_CONSTANT_RELATION) {
					result->type = MONO_VARIABLE_RELATION;
					result->value.variable.variable = left_value.value.variable.variable;
					result->value.variable.delta = left_value.value.variable.delta - right_value.value.constant.value;
				} else {
					MAKE_VALUE_ANY (*result);
				}
			} else if ((right_value.type == MONO_CONSTANT_RELATION) && (left_value.type == MONO_CONSTANT_RELATION)) {
				result->type = MONO_CONSTANT_RELATION;
				result->value.constant.value = left_value.value.constant.value - right_value.value.constant.value;
			} else {
				MAKE_VALUE_ANY (*result);
			}
		
		    if (result->type == MONO_VARIABLE_RELATION) {
				check_delta_safety (area, result);
			}
			
		} else {
			MAKE_VALUE_ANY (*result);
		}
		break;
	}
	case CEE_AND: {
		if (value_context == MONO_INTEGER_CONTEXT){
			int constant_operand_value;
			process_inst (inst->inst_left, bb, area, &left_value, bb_scope, MONO_INTEGER_CONTEXT, result_value_kind, &value_kind);
			process_inst (inst->inst_right, bb, area, &right_value, bb_scope, MONO_INTEGER_CONTEXT, result_value_kind, &value_kind);

		    if (left_value.type == MONO_CONSTANT_RELATION) {
				constant_operand_value = left_value.value.constant.value;
			} else if (right_value.type == MONO_CONSTANT_RELATION) {
				constant_operand_value = right_value.value.constant.value;
			} else {
				constant_operand_value = 0;
			}
		
		    if (constant_operand_value > 0) {
			    if (constant_operand_value <= 0xff) {
				    if ((result_value_kind & MONO_INTEGER_VALUE_SIZE_BITMASK) > 1) {
						*p_value_kind = MONO_UNSIGNED_INTEGER_VALUE_SIZE_1;
					}
			    } else if (constant_operand_value <= 0xffff) {
				    if ((result_value_kind & MONO_INTEGER_VALUE_SIZE_BITMASK) > 2) {
						*p_value_kind = MONO_UNSIGNED_INTEGER_VALUE_SIZE_2;
					}
				}
			}
		
			MAKE_VALUE_ANY (*result);
			
		} else {
			MAKE_VALUE_ANY (*result);
		}
		break;
	}
	case CEE_CONV_I1:
	case CEE_CONV_OVF_I1:
	case CEE_CONV_OVF_I1_UN:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_INTEGER_VALUE_SIZE_1;
		MAKE_VALUE_ANY (*result);
		break;
	case CEE_CONV_U1:
	case CEE_CONV_OVF_U1:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_UNSIGNED_INTEGER_VALUE_SIZE_1;
		MAKE_VALUE_ANY (*result);
		break;
	case CEE_CONV_I2:
	case CEE_CONV_OVF_I2:
	case CEE_CONV_OVF_I2_UN:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_INTEGER_VALUE_SIZE_2;
		MAKE_VALUE_ANY (*result);
	    break;
	case CEE_CONV_U2:
	case CEE_CONV_OVF_U2:
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_UNSIGNED_INTEGER_VALUE_SIZE_2;
		MAKE_VALUE_ANY (*result);
		break;
	case CEE_LDLEN:
		process_inst (inst->inst_left, bb, area, result, bb_scope, MONO_ARRAY_CONTEXT, TRUE);
		if (value_context == MONO_INTEGER_CONTEXT)
			*p_value_kind = MONO_UNSIGNED_INTEGER_VALUE_SIZE_4;
		break;
	case OP_COMPARE: {
		if (value_context == MONO_CONDITIONALBRANCH_CONTEXT) {

			MonoValueRelation symmetric_relation;
			int variable_index;

			symmetric_relation = MONO_SYMMETRIC_RELATION (branch_relation);
					
			process_inst (inst->inst_left, bb, area, &left_value, bb_scope, MONO_INTEGER_CONTEXT, MONO_UNKNOWN_INTEGER_VALUE, &value_kind);
			process_inst (inst->inst_right, bb, area, &right_value, bb_scope, MONO_INTEGER_CONTEXT, MONO_UNKNOWN_INTEGER_VALUE, &value_kind);
			
			if ((left_value.type == MONO_VARIABLE_RELATION) && ((right_value.type == MONO_VARIABLE_RELATION)||(right_value.type == MONO_CONSTANT_RELATION))) {
				variable_index = left_value.value.variable.variable;
				known_fact = add_new_knownfact_to_evaluation_area (area, bb->block_num, variable_index);
				known_fact->relation_is_static_definition = FALSE;
			    if (right_value.type == MONO_CONSTANT_RELATION) {
					known_fact->variable_index = variable_index;
					known_fact->type = MONO_CONSTANT_RELATION;
					known_fact->value.constant.relation = branch_relation;
					known_fact->value.constant.value = right_value.value.constant.value - left_value.value.variable.delta;
				} else {
					known_fact->variable_index = variable_index;
					known_fact->type = MONO_VARIABLE_RELATION;
					known_fact->value.variable.relation = branch_relation;
					known_fact->value.variable.variable = right_value.value.variable.variable;
					known_fact->value.variable.delta = right_value.value.variable.delta - left_value.value.variable.delta;
				}
				
				if (TRACE_RC_REMOVAL) {
					printf ("Adding relation on variable %d for BB%d: ", variable_index, bb->block_num);
					print_summarized_value_relation (known_fact);
					printf ("\n");					
				}
			}
			if ((right_value.type == MONO_VARIABLE_RELATION) && ((left_value.type == MONO_VARIABLE_RELATION)||(left_value.type == MONO_CONSTANT_RELATION))) {
				variable_index = right_value.value.variable.variable;
				known_fact = add_new_knownfact_to_evaluation_area (area, bb->block_num, variable_index);
				known_fact->relation_is_static_definition = FALSE;
			    if (left_value.type == MONO_CONSTANT_RELATION) {
					known_fact->variable_index = variable_index;
					known_fact->type = MONO_CONSTANT_RELATION;
					known_fact->value.constant.relation = symmetric_relation;
					known_fact->value.constant.value = left_value.value.constant.value - right_value.value.variable.delta;
				} else {
					known_fact->variable_index = variable_index;
					known_fact->type = MONO_VARIABLE_RELATION;
					known_fact->value.variable.relation = symmetric_relation;
					known_fact->value.variable.variable = left_value.value.variable.variable;
					known_fact->value.variable.delta = left_value.value.variable.delta - right_value.value.variable.delta;
				}
				if (TRACE_RC_REMOVAL) {
					printf ("Adding relation on variable %d fo BB%d: ", variable_index, bb->block_num);
					print_summarized_value_relation (known_fact);
					printf ("\n");
				}
			}
				
		} else {
			MONO_INST_ARITY (inst);
		}
		break;
	}
	default:
		MONO_INST_ARITY (inst);
	        MAKE_VALUE_ANY (*result);
	}
}


/*
 * Process a Basic Block.
 * It does the following (in sequence):
 * - Get the BB entry condition
 * - Process all the MonoInst trees in the BB
 * - Recursively process all the children BBs in the dominator tree
 * - Remove the known facts previously added to the relation graph for the current BB
 *
 * bb: the BB that must be processed
 * area: the current evaluation area (it contains the relation graph and
 *       memory for all the evaluation contexts is already allocated)
 */
static void
process_block (MonoBasicBlock *bb, MonoKnownFactsEvaluationArea *area)
{
	int inst_index;
	MonoInst *current_inst;
	MonoKnownFact result;
	GList *dominated_bb;
	MonoKnownFact *bb_known_fact;
	MonoBasicBlock *in_bb;
	MonoInst *branch;


	if (TRACE_RC_REMOVAL) {
		printf ("Processing block %d [dfn %d]...\n", bb->block_num, bb->dfn);
	}
	
	if (bb->in_count == 1) {
		if (TRACE_RC_REMOVAL) {
			printf ("Getting relations from previuos BB\n");
		}
		in_bb = bb->in_bb [0];
		branch = in_bb->last_ins;
		if (branch != NULL) {
			MonoValueRelation branch_relation;
			gboolean code_path;
			
			switch (branch->opcode) {
			case CEE_BEQ:
			case CEE_BLT:
			case CEE_BLT_UN:
			case CEE_BLE:
			case CEE_BLE_UN:
			case CEE_BGT:
			case CEE_BGT_UN:
			case CEE_BGE:
			case CEE_BGE_UN:
			case CEE_BNE_UN:
				branch_relation = get_relation_from_branch_instruction (branch->opcode);
				if ((branch_relation != MONO_ANY_RELATION) && (branch->inst_left->opcode == OP_COMPARE)) {
				    if (branch->inst_true_bb == bb) {
						code_path = TRUE;
					} else if (branch->inst_false_bb == bb) {
						code_path = FALSE;
					} else {
						code_path = TRUE;
						g_assert_not_reached ();
					}					
					if (!code_path) 
						branch_relation = MONO_NEGATED_RELATION (branch_relation);
					
					process_inst(branch->inst_left, bb, area, &result, TRUE, MONO_CONDITIONALBRANCH_CONTEXT, branch_relation);
				}
				break;
			default:
				if (TRACE_RC_REMOVAL) {
					printf ("The instruction haven't a conditional branch\n");
				}
			}
		}	
	}

	inst_index = 0;
	current_inst = bb->code;
	while (current_inst != NULL)
	{
		if (TRACE_RC_REMOVAL) {
			printf ("Processing instruction %d\n", inst_index);
			inst_index++;
		}
				
		process_inst (current_inst, bb, area, &result, TRUE, MONO_ROOT_CONTEXT);		
		current_inst = current_inst->next;
	}


	if (TRACE_RC_REMOVAL) {
		printf ("Processing block %d [dfn %d] done.\n", bb->block_num, bb->dfn);
	}

	for (dominated_bb = g_list_first (bb->dominated); dominated_bb != NULL; dominated_bb = g_list_next (dominated_bb)) {
		process_block ((MonoBasicBlock *) (dominated_bb->data), area);
	}

	if (TRACE_RC_REMOVAL) {
		printf("Exit from block %d\n", bb->block_num);
	}


	for (bb_known_fact = area->bb_known_facts [bb->block_num]; bb_known_fact != NULL; bb_known_fact = bb_known_fact->next_known_fact_in_BB){
		remove_knownfact_from_evaluation_area (area, bb_known_fact);
	}

}



/**
 * mono_perform_rc_removal:
 * @cfg: Control Flow Graph
 *
 * Performs the redundant checks removal from a cfg in SSA form.
 * It does the following:
 * - Prepare the evaluation area
 * - Allocate memory for the relation graph in the evaluation area
 *   (of course, only for variable definitions) and summarize there all
 *   variable definitions
 * - Allocate memory for the evaluation contexts in the evaluation area
 * - Recursively process all the BBs in the dominator tree (it is enough
 *   to invoke the processing on the entry BB)
 * 
 * cfg: the method code
 */
void
mono_perform_rc_removal (MonoCompile *cfg)
{
	MonoKnownFactsEvaluationArea area;
	MonoKnownFact result;
	int i;
	
	verbose_level = cfg->verbose_level;
	
	if (TRACE_RC_REMOVAL) {
		printf ("Removing redundant checks in %s\n", mono_method_full_name (cfg->method, TRUE));
	}
	
	area.cfg = cfg;
	area.mempool = mono_mempool_new ();
	area.known_facts = (MonoKnownFact**) 
	    mono_mempool_alloc0 (area.mempool, sizeof (MonoKnownFact) * (cfg->num_varinfo));
	area.bb_known_facts = (MonoKnownFact**) 
	    mono_mempool_alloc0 (area.mempool, sizeof (MonoKnownFact) * (cfg->num_bblocks));															  
	area.contexts= (MonoKnownFactsEvaluationContext*) 
        mono_mempool_alloc0 (area.mempool, sizeof (MonoKnownFactsEvaluationContext) * (cfg->num_varinfo));
	area.variable_value_kind = (MonoIntegerValueKind*) 
	    mono_mempool_alloc0 (area.mempool, sizeof (MonoIntegerValueKind) * (cfg->num_varinfo));
	
	
	for (i = 0; i < cfg->num_varinfo; i++) {
		
		area.variable_value_kind [i] = MONO_UNKNOWN_INTEGER_VALUE;
		
		if (cfg->vars [i]->def != NULL) {			
			process_inst (cfg->vars [i]->def, NULL, &area, &result, FALSE, MONO_ROOT_CONTEXT);
		} else {
			if (TRACE_RC_REMOVAL) {
				printf ("Variable %d has no definition, probably it is an argument\n", i);
			}
		}
	}
			
	process_block (cfg->bblocks [0], &area);

}
