/*
 * rcr.h: Redundant check removal
 *
 * Author:
 *   Massimiliano Mantione (massi@ximian.com)
 *   Gianluigi Spagnuolo
 *
 * (C) 2004 Ximian, Inc.  http://www.ximian.com
 */

#ifndef __MONO_RCR_H__
#define __MONO_RCR_H__

#include <limits.h>

#include "mini.h"
#include <mono/metadata/mempool.h>

/**
 * A "relation" between two values.
 * The enumeration is used as a bit field, with three significant bits.
 * The degenerated cases are meaningful:
 * MONO_ANY_RELATION: we know nothing of this relation
 * MONO_NO_RELATION: no relation is possible (this code is unreachable)
 */
typedef enum {
	MONO_EQ_RELATION = 1,
	MONO_LT_RELATION = 2,
	MONO_GT_RELATION = 4,
	MONO_NE_RELATION = (MONO_LT_RELATION|MONO_GT_RELATION),
	MONO_LE_RELATION = (MONO_LT_RELATION|MONO_EQ_RELATION),
	MONO_GE_RELATION = (MONO_GT_RELATION|MONO_EQ_RELATION),
	MONO_ANY_RELATION = (MONO_EQ_RELATION|MONO_LT_RELATION|MONO_GT_RELATION),
	MONO_NO_RELATION = 0
} MonoValueRelation;

/**
 * Type of "relation" with a class.
 */
typedef enum {
	MONO_IS_ISTANCE_OF,
	MONO_IS_NOT_ISTANCE_OF
} MonoClassRelation;

/**
 * All handled value types (expressions) in variable definitions and branch
 * conditions:
 * CONSTANT: an integer constant
 * VARIABLE: a reference to a variable, with an optional delta (can be zero)
 * PHI: a PHI definition of the SSA representation
 * CLASS: a reference to a class
 * EMPTY: 
 */
typedef enum {
	MONO_CONSTANT_RELATION,
	MONO_VARIABLE_RELATION,
	MONO_PHI_RELATION,
	MONO_CLASS_RELATION,
	MONO_EMPTY_RELATION
} MonoKnownFactType;


/** A context related to a known fact
 * ROOT:
 * INTEGER:
 * ARRAY:
 * REFERENCE: 
 * CONDITIONALBRANCH:
 * UNKNOWN: 
 */
typedef enum {
	MONO_ROOT_CONTEXT,
	MONO_INTEGER_CONTEXT,
	MONO_ARRAY_CONTEXT,
	MONO_REFERENCE_CONTEXT,
	MONO_CONDITIONALBRANCH_CONTEXT,
	MONO_UNKNOWN_CONTEXT
} MonoValueContext;

/**
 * A MONO_CONSTANT_RELATION value.
 * relation: the relation between the variable and the constant value
 * value: the value
 */
typedef struct MonoRelationWithConstant {
	MonoValueRelation relation;
	int value;
} MonoRelationWithConstant;

/**
 * A MONO_VARIABLE_RELATION value
 * relation: the relation between the variable and the value
 * variable: the variable index in the cfg
 * delta: the delta (can be zero)
 */
typedef struct MonoRelationWithVariable {
	MonoValueRelation relation;
	int variable;
	int delta;
} MonoRelationWithVariable;

/**
 * A MONO_CLASS_RELATION value
 * relation: the relation between the variable and the class
 * class: the class involved in the relation
 */
typedef struct MonoRelationWithClass {
	MonoClassRelation relation;
	MonoClass *klass;
} MonoRelationWithClass;

/**
 * A MONO_PHI_RELATION value.
 * relation:  the relation between the variable and the value
 * number_of_alternatives: the number of alternatives in the PHI definition
 * phi_alternatives: an array of integers with the indexes of the variables
 *                   which are the alternatives in this PHI definition
 */
typedef struct MonoSummarizedPhiValue {
	MonoValueRelation relation;
	int number_of_alternatives;
	int *phi_alternatives;
} MonoRelationWithPhi;

/**
 * A "kind" of integer value.
 * The enumeration is used as a bit field, with two fields.
 * The first, four bits wide, is the "sizeof" in bytes.
 * The second is a flag that is true if the value is unsigned.
 */
typedef enum {
	MONO_INTEGER_VALUE_SIZE_1 = 1,
	MONO_INTEGER_VALUE_SIZE_2 = 2,
	MONO_INTEGER_VALUE_SIZE_4 = 4,
	MONO_INTEGER_VALUE_SIZE_8 = 8,
	MONO_INTEGER_VALUE_SIZE_BITMASK = 15,
	MONO_UNSIGNED_VALUE_FLAG = 16,
	MONO_UNSIGNED_INTEGER_VALUE_SIZE_1 = MONO_UNSIGNED_VALUE_FLAG|MONO_INTEGER_VALUE_SIZE_1,
	MONO_UNSIGNED_INTEGER_VALUE_SIZE_2 = MONO_UNSIGNED_VALUE_FLAG|MONO_INTEGER_VALUE_SIZE_2,
	MONO_UNSIGNED_INTEGER_VALUE_SIZE_4 = MONO_UNSIGNED_VALUE_FLAG|MONO_INTEGER_VALUE_SIZE_4,
	MONO_UNSIGNED_INTEGER_VALUE_SIZE_8 = MONO_UNSIGNED_VALUE_FLAG|MONO_INTEGER_VALUE_SIZE_8,
	MONO_UNKNOWN_INTEGER_VALUE = 0
} MonoIntegerValueKind;

/**
 * A relation between variables (or a variable and a constant, or a variable
 * and a class). 
 * type: the type of relation
 * value: the related value
 * variable index: the first variable of the relation
 * relation_is_static_definition: TRUE if the relation comes from a variable
 *                                definition, FALSE if it comes from a branch
 *                                condition
 * index: index of the first variable
 * next_known_fact_in_BB: pointer to the next relation of this variable added in 
 *                      the current BB
 * prev: pointer to the previous relation of this variable in the evaluation area
 * next: pointer to the next relation of this variable in the evaluation area
 *
 * NOTE: for each variable are defined two lists, one for the relations added 
 * in the current BB (next_relation_in_BB pointer), and another doubly linked
 * list for all the relations related to a variable (prev and next pointers).
 */
typedef struct MonoKnownFact {
	MonoKnownFactType type;
	union {
		MonoRelationWithConstant constant;
		MonoRelationWithVariable variable;
		MonoRelationWithPhi phi;
		MonoRelationWithClass klass;
	} value;
	int variable_index;
	gboolean relation_is_static_definition;
	struct MonoKnownFact *next_known_fact_in_BB;
	struct MonoKnownFact *next;
	struct MonoKnownFact *prev;
} MonoKnownFact;

/**
 * The evaluation status for one variable.
 * The enumeration is used as a bit field, because the status has two
 * distinct sections.
 * The first is the "main" one (bits 0, 1 and 2), which is actually a proper
 * enumeration (the bits are mutually exclusive, and their meaning is obvious).
 * The other section (the bits in the MONO_RELATIONS_EVALUATION_IS_RECURSIVE
 * set) is used to mark an evaluation as recursive (while backtracking through
 * the evaluation contexts), to state if the graph loop gives a value that is
 * ascending, descending or indefinite.
 * The bits are handled separately because the same evaluation context could
 * belong to more than one loop, so that each loop would set its bits.
 * After the backtracking, the bits are examined and a decision is taken.
 * 
 */
typedef enum {
	MONO_KNOWNFACTS_EVALUATION_NOT_STARTED = 0,
	MONO_KNOWNFACTS_EVALUATION_IN_PROGRESS = 1,
	MONO_KNOWNFACTS_EVALUATION_COMPLETED = 2,
	MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_ASCENDING = 4,
	MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_DESCENDING = 8,
	MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_INDEFINITE = 16,
	MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVE = (MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_ASCENDING|MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_DESCENDING|MONO_KNOWNFACTS_EVALUATION_IS_RECURSIVELY_INDEFINITE)
} MonoKnownFactsEvaluationStatus;

/**
 * A range of values (ranges include their limits).
 * A range from MIN_INT to MAX_INT is "indefinite" (any value).
 * A range where upper < lower means unreachable code (some of the relations
 * that generated the range is incompatible, like x = 0 and x > 0).
 * lower: the lower limit
 * upper: the upper limit
 */
typedef struct MonoKnownFactsEvaluationRange {
	int lower;
	int upper;
} MonoKnownFactsEvaluationRange;

/**
 * The two ranges that contain the result of a variable evaluation.
 * zero: the range with respect to zero
 * variable: the range with respect to the target variable in this evaluation
 */
typedef struct MonoKnownFactsEvaluationRanges {
	MonoKnownFactsEvaluationRange zero;
	MonoKnownFactsEvaluationRange variable;
} MonoKnownFactsEvaluationRanges;

/**
 * The context of a variable evaluation.
 * status: the evaluation status
 * current_relation: the relation that is currently evaluated.
 * ranges: the result of the evaluation.
 * father: the context of the evaluation that invoked this one (used to
 *         perform the backtracking when loops are detected.
 */
typedef struct MonoKnownFactsEvaluationContext {
	MonoKnownFactsEvaluationStatus status;
	MonoKnownFact *current_relation;
	MonoKnownFactsEvaluationRanges ranges;
	struct MonoKnownFactsEvaluationContext *father;
} MonoKnownFactsEvaluationContext;

/*
 * Basic macros to initialize and check ranges.
 */
#define MONO_MAKE_RELATIONS_EVALUATION_RANGE_WEAK(r) do{\
		(r).lower = INT_MIN;\
		(r).upper = INT_MAX;\
	} while (0)
#define MONO_MAKE_RELATIONS_EVALUATION_RANGES_WEAK(rs) do{\
		MONO_MAKE_RELATIONS_EVALUATION_RANGE_WEAK ((rs).zero); \
		MONO_MAKE_RELATIONS_EVALUATION_RANGE_WEAK ((rs).variable); \
	} while (0)
#define MONO_MAKE_RELATIONS_EVALUATION_RANGE_IMPOSSIBLE(r) do{\
		(r).lower = INT_MAX;\
		(r).upper = INT_MIN;\
	} while (0)
#define MONO_MAKE_RELATIONS_EVALUATION_RANGES_IMPOSSIBLE(rs) do{\
		MONO_MAKE_RELATIONS_EVALUATION_RANGE_IMPOSSIBLE ((rs).zero); \
		MONO_MAKE_RELATIONS_EVALUATION_RANGE_IMPOSSIBLE ((rs).variable); \
	} while (0)
#define MONO_RELATIONS_EVALUATION_RANGE_IS_WEAK(r) (((r).lower==INT_MIN)&&((r).upper==INT_MAX))
#define MONO_RELATIONS_EVALUATION_RANGES_ARE_WEAK(rs) \
	(MONO_RELATIONS_EVALUATION_RANGE_IS_WEAK((rs).zero) && \
	MONO_RELATIONS_EVALUATION_RANGE_IS_WEAK((rs).variable))
#define MONO_RELATIONS_EVALUATION_RANGE_IS_IMPOSSIBLE(r) (((r).lower)>((r).upper))
#define MONO_RELATIONS_EVALUATION_RANGES_ARE_IMPOSSIBLE(rs) \
	(MONO_RELATIONS_EVALUATION_RANGE_IS_IMPOSSIBLE((rs).zero) || \
	MONO_RELATIONS_EVALUATION_RANGE_IS_IMPOSSIBLE((rs).variable))

/*
 * The following macros are needed because ranges include theit limits, but
 * some relations explicitly exclude them (GT and LT).
 */
#define MONO_UPPER_EVALUATION_RANGE_NOT_EQUAL(ur) ((((ur)==INT_MIN)||((ur)==INT_MAX))?(ur):((ur)-1))
#define MONO_LOWER_EVALUATION_RANGE_NOT_EQUAL(lr) ((((lr)==INT_MIN)||((lr)==INT_MAX))?(lr):((lr)+1))
#define MONO_APPLY_INEQUALITY_TO_EVALUATION_RANGE(r) do{\
		(r).lower = MONO_LOWER_EVALUATION_RANGE_NOT_EQUAL ((r).lower);\
		(r).upper = MONO_UPPER_EVALUATION_RANGE_NOT_EQUAL ((r).upper);\
	} while (0)
#define MONO_APPLY_INEQUALITY_TO_EVALUATION_RANGES(rs) do{\
		MONO_APPLY_INEQUALITY_TO_EVALUATION_RANGE ((rs).zero); \
		MONO_APPLY_INEQUALITY_TO_EVALUATION_RANGE ((rs).variable); \
	} while (0)

/*
 * The following macros perform union and intersection operations on ranges.
 */
#define MONO_LOWER_EVALUATION_RANGE_UNION(lr,other_lr) ((lr)=MIN(lr,other_lr))
#define MONO_UPPER_EVALUATION_RANGE_UNION(ur,other_ur) ((ur)=MAX(ur,other_ur))
#define MONO_LOWER_EVALUATION_RANGE_INTERSECTION(lr,other_lr) ((lr)=MAX(lr,other_lr))
#define MONO_UPPER_EVALUATION_RANGE_INTERSECTION(ur,other_ur) ((ur)=MIN(ur,other_ur))
#define MONO_RELATIONS_EVALUATION_RANGE_UNION(r,other_r) do{\
		MONO_LOWER_EVALUATION_RANGE_UNION((r).lower,(other_r).lower);\
		MONO_UPPER_EVALUATION_RANGE_UNION((r).upper,(other_r).upper);\
	} while (0)
#define MONO_RELATIONS_EVALUATION_RANGE_INTERSECTION(r,other_r) do{\
		MONO_LOWER_EVALUATION_RANGE_INTERSECTION((r).lower,(other_r).lower);\
		MONO_UPPER_EVALUATION_RANGE_INTERSECTION((r).upper,(other_r).upper);\
	} while (0)
#define MONO_RELATIONS_EVALUATION_RANGES_UNION(rs,other_rs) do{\
		MONO_RELATIONS_EVALUATION_RANGE_UNION ((rs).zero,(other_rs).zero); \
		MONO_RELATIONS_EVALUATION_RANGE_UNION ((rs).variable,(other_rs).variable); \
	} while (0)
#define MONO_RELATIONS_EVALUATION_RANGES_INTERSECTION(rs,other_rs) do{\
		MONO_RELATIONS_EVALUATION_RANGE_INTERSECTION ((rs).zero,(other_rs).zero); \
		MONO_RELATIONS_EVALUATION_RANGE_INTERSECTION ((rs).variable,(other_rs).variable); \
	} while (0)

/*
 * The following macros add or subtract "safely" (without over/under-flow) a
 * delta (constant) value to a range.
 */
#define MONO_ADD_DELTA_SAFELY(v,d) do{\
		if (((d) > 0) && ((v) != INT_MIN)) {\
			(v) = (((v)+(d))>(v))?((v)+(d)):INT_MAX;\
		} else if (((d) < 0) && ((v) != INT_MAX)) {\
			(v) = (((v)+(d))<(v))?((v)+(d)):INT_MIN;\
		}\
	} while (0)
#define MONO_SUB_DELTA_SAFELY(v,d) do{\
		if (((d) < 0) && ((v) != INT_MIN)) {\
			(v) = (((v)-(d))>(v))?((v)-(d)):INT_MAX;\
		} else if (((d) > 0) && ((v) != INT_MAX)) {\
			(v) = (((v)-(d))<(v))?((v)-(d)):INT_MIN;\
		}\
	} while (0)
#define MONO_ADD_DELTA_SAFELY_TO_RANGE(r,d) do{\
		MONO_ADD_DELTA_SAFELY((r).lower,(d));\
		MONO_ADD_DELTA_SAFELY((r).upper,(d));\
	} while (0)
#define MONO_SUB_DELTA_SAFELY_FROM_RANGE(r,d) do{\
		MONO_SUB_DELTA_SAFELY((r).lower,(d));\
		MONO_SUB_DELTA_SAFELY((r).upper,(d));\
	} while (0)
#define MONO_ADD_DELTA_SAFELY_TO_RANGES(rs,d) do{\
		MONO_ADD_DELTA_SAFELY_TO_RANGE((rs).zero,(d));\
		MONO_ADD_DELTA_SAFELY_TO_RANGE((rs).variable,(d));\
	} while (0)
#define MONO_SUB_DELTA_SAFELY_FROM_RANGES(rs,d) do{\
		MONO_SUB_DELTA_SAFELY_FROM_RANGE((rs).zero,(d));\
		MONO_SUB_DELTA_SAFELY_FROM_RANGE((rs).variable,(d));\
	} while (0)


/**
 * The main evaluation area.
 * cfg: the cfg of the method that is being examined.
 * known_facts: array of known facts, one for each method variable (each
 *            known fact is the head of a list); this is the evaluation graph
 * bb_known_facts: array of known facts, one for each basic block (each
 *            known facts is the head of a list)	
 * contexts: an array of evaluation contexts (one for each method variable)
 * variable_value_kind: an array of MonoIntegerValueKind, one for each local
 *                      variable (or argument)
 */
typedef struct MonoKnownFactsEvaluationArea {
	MonoCompile *cfg;
	MonoMemPool *mempool;
	MonoKnownFact **known_facts;
	MonoKnownFact **bb_known_facts;
	MonoKnownFactsEvaluationContext *contexts;
	MonoIntegerValueKind *variable_value_kind;
} MonoKnownFactsEvaluationArea;


#endif /* __MONO_RCR_H__ */
