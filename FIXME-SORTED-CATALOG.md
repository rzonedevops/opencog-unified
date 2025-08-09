# OpenCog Unified FIXME Implementation Catalog

## Executive Summary

This document provides a comprehensive categorization of all **324 FIXME instances** found in the OpenCog Unified repository, sorted by implementation difficulty to guide development priorities and resource allocation.

### Summary Statistics

| Difficulty Level | Count | Percentage | Estimated Total Effort |
|-----------------|-------|------------|------------------------|
| **VERY_HARD** | 36 | 11.1% | 6-18 months |
| **HARD** | 33 | 10.2% | 2-8 months |  
| **MEDIUM** | 240 | 74.1% | 3-12 months |
| **EASY** | 15 | 4.6% | 1-4 weeks |

**Total files affected**: 143

---

## Implementation Priority Recommendations

### 🔥 Critical Priority (VERY_HARD)
These items require specialized expertise and significant architectural work. Consider outsourcing to domain experts or dedicating senior developers full-time.

### ⚡ High Priority (HARD) 
These items impact core functionality, performance, and reliability. Should be addressed by experienced developers with relevant domain knowledge.

### 📋 Medium Priority (MEDIUM)
Standard implementation work that can be distributed among team members based on component expertise.

### ✅ Easy Wins (EASY)
Quick fixes suitable for new contributors or as warm-up tasks.

---


## 🚨 Very Hard Priority (33 items)

### Atomspace Component (12 items)


**1.** `atomspace/opencog/atomspace/AtomSpace.h:524`

**Issue:** XXX FIXME Users should call StorageNode::add_nocheck() instead.

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
/* ----------------------------------------------------------- */
// Not for public use! Only StorageNodes get to call this!
// XXX FIXME Users should call StorageNode::add_nocheck() instead.
Handle storage_add_nocheck(const Handle& h) { return add(h); }
};
```
</details>


**2.** `atomspace/opencog/guile/SchemeSmobValue.cc:313`

**Issue:** XXX FIXME... at this time, nodes have a single name.

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
if (nameserver().isA(t, NODE))
{
// XXX FIXME... at this time, nodes have a single name.
SCM sname = SCM_CAR(svalue_list);
std::string name = verify_string(sname, "cog-new-value", 2);
```
</details>


**3.** `atomspace/opencog/query/InitiateSearchMixin.cc:126`

**Issue:** XXX FIXMEwe should be using ptm->isVariable() instead !?

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
if (_nameserver.isNode(t))
{
// XXX FIXME; we should be using ptm->isVariable() instead !?
if (VARIABLE_NODE != t and GLOB_NODE != t and SIGN_NODE != t)
{
```
</details>


**4.** `atomspace/opencog/sheaf/attic/linear-parser.scm:156`

**Issue:** XXX FIXME WARNING DANGER: As written, this runs in exponential time

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
is simply connected.

XXX FIXME WARNING DANGER: As written, this runs in exponential time
as the size of the graph (the wedges), and thus can explode in
runtime, going from a fraction of a second for one graph, and many
```
</details>


**5.** `atomspace/opencog/sheaf/attic/vo-graph.scm:232`

**Issue:** XXX FIXME WARNING DANGER: As written, this runs in exponential time

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
end of it.

XXX FIXME WARNING DANGER: As written, this runs in exponential time
as the size of the graph (the WELI), and thus can explode in
runtime, going from a fraction of a second for one graph, and many
```
</details>


**6.** `atomspace/opencog/sheaf/attic/vo-graph.scm:290`

**Issue:** XXX FIXME WARNING DANGER: As written, this runs in exponential time

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
end of it.

XXX FIXME WARNING DANGER: As written, this runs in exponential time
as the size of the graph (the WELI), and thus can explode in
runtime, going from a fraction of a second for one graph, and many
```
</details>


**7.** `atomspace/opencog/atoms/pattern/PatternUtils.cc:55`

**Issue:** XXX FIXME Are the below needed?

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
EVALUATABLE_LINK)))

// XXX FIXME Are the below needed?
or contains_atomtype(clause, DEFINED_PREDICATE_NODE)
or contains_atomtype(clause, DEFINED_SCHEMA_NODE)
```
</details>


**8.** `atomspace/opencog/atoms/core/Checkers.cc:78`

**Issue:** XXX FIXME, this is to be removed, because UnionLink,

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
// union and complement. Since it cannot inherit from
// EVALUATABLE_LINK (cause it's a Node) we have to add it here.
// XXX FIXME, this is to be removed, because UnionLink,
// IntersectionLink takes the place of OrLink, AndLink.
if (h->is_type(CONCEPT_NODE)) continue;
```
</details>


**9.** `atomspace/opencog/atoms/core/TypedVariableLink.cc:50`

**Issue:** which should be using a SignatureLink for this case. XXX FIXME.

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```

// Allow VARIABLE_NODE, although this is a bug in the URE,
// which should be using a SignatureLink for this case. XXX FIXME.
Type dtype = _outgoing[1]->get_type();
if (not nameserver().isA(dtype, TYPE_NODE) and
```
</details>


**10.** `atomspace/opencog/atoms/core/TypedVariableLink.cc:56`

**Issue:** VARIABLE_NODE != dtype and XXX FIXME this is wrongURE-bug

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
TYPE_CHOICE != dtype and
TYPE_INTERSECTION_LINK != dtype and
VARIABLE_NODE != dtype and // XXX FIXME this is wrong; URE-bug
SIGNATURE_LINK != dtype and
INTERVAL_LINK != dtype and
```
</details>


**11.** `atomspace/opencog/atoms/reduct/DecimateLink.cc:65`

**Issue:** XXX FIXME ... both the NumberNode and the FloatValue variations

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME ... both the NumberNode and the FloatValue variations
// below make a copy of the mask.  Instead of making a copy, create
// something more efficient/faster. It is, after all, a simple
```
</details>


**12.** `atomspace/tests/cython/atomspace/test_atomspace.py:362`

**Issue:** XXX FIXME is testing the name of the bottom type

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
self.assertEqual(get_type_name(types.Node), "Node")
self.assertEqual(get_type_name(2231), "")
# XXX FIXME is testing the name of the bottom type
# a sane thing to do?
self.assertEqual(get_type_name(types.NO_TYPE), "*** Bottom Type! ***")
```
</details>


**13.** `atomspace/opencog/atomspace/TypeIndex.h:73`

**Issue:** TODO The iterator is NOT thread-safe against the insertion or removal of atoms!

**Category:** Threading/Concurrency  
**Effort:** 2-6 months  
**Reasoning:** Requires thread safety expertise and careful concurrency implementation

<details>
<summary>View Code Context</summary>

```
 * faster.
 *
 * @todo The iterator is NOT thread-safe against the insertion or
 * removal of atoms!  Either inserting or removing an atom will cause
 * the iterator references to be freed, leading to mystery crashes!
```
</details>


**14.** `atomspace/opencog/atoms/base/Valuation.cc:50`

**Issue:** TODO C++ smart pointers are not atomic; we really need to use a lock here

**Category:** Threading/Concurrency  
**Effort:** 2-6 months  
**Reasoning:** Requires atomic operations and thread-safe memory management expertise

<details>
<summary>View Code Context</summary>

```
void Valuation::setValue(const ValuePtr& v)
{
	// XXX TODO -- C++ smart pointers are not atomic; we really
	// need to use a lock here, to avoid thread-races.
	_value = v;
}
```
</details>


**15.** `atomspace/opencog/atoms/parallel/ExecuteThreadedLink.cc:59`

**Issue:** TODO We could have a non-blocking version of this atom

**Category:** Threading/Concurrency  
**Effort:** 2-6 months  
**Reasoning:** Requires expertise in concurrent programming and non-blocking algorithms

<details>
<summary>View Code Context</summary>

```
/// Atoms in the set. If the NumberNode is present, then the number of
/// threads is the smaller of the NumberNode and the seize of the Set.
///
/// XXX TODO: We could have a non-blocking version of this atom. We
/// could just return the QueueValue immediately; the user could check
/// to see if the queue is closed, to find out if the threads have
/// finished.
```
</details>


### Atomspace-Storage Component (2 items)


**1.** `atomspace-storage/opencog/persist/flow/StoreValueOfLink.cc:61`

**Issue:** XXX TODO FIXME ... if either of these are executable, then

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
StorageNodePtr stnp = StorageNodeCast(_outgoing[2]);

// XXX TODO FIXME ... if either of these are executable, then
// they need to be executed, first, right? Because that's the
// usual intent. Else they'd be wrapped in a DontExecLink, right?
```
</details>


**2.** `atomspace-storage/opencog/persist/flow/FetchValueOfLink.cc:63`

**Issue:** XXX TODO FIXME ... if either of _outgoing[0] or _outgoing[1]

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
StorageNodePtr stnp = StorageNodeCast(_outgoing[2]);

// XXX TODO FIXME ... if either of _outgoing[0] or _outgoing[1]
// are executable, then they need to be executed, first, right?
// Yes, they do. But, for just right now, we don't, to stay
```
</details>


### Cogutil Component (2 items)


**1.** `cogutil/opencog/util/tree.h:878`

**Issue:** tree_assert(1==0)FIXME: not correct yet

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
typename tree<T, tree_node_allocator>::fixed_depth_iterator tree<T, tree_node_allocator>::end_fixed(const iterator_base& pos, unsigned int dp) const
{
tree_assert(1==0); // FIXME: not correct yet
tree_node *tmp=pos.node;
unsigned int curdepth=1;
```
</details>


**2.** `cogutil/opencog/util/tree.h:2693`

**Issue:** returnFIXME: see 'set_first_parent()'

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
void tree<T, tree_node_allocator>::fixed_depth_iterator::find_leftmost_parent_()
{
return; // FIXME: see 'set_first_parent()'
tree_node *tmppar=first_parent_;
while(tmppar->prev_sibling) {
```
</details>


### Components Component (1 items)


**1.** `components/learning/moses/moses/moses/scoring/bscores.cc:930`

**Issue:** /XXX this should probably be removed! TODO FIXME

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
/// formula (the edges, with are printed by hand, below).
///
/// XXX this should probably be removed! TODO FIXME

cluster_bscore::cluster_bscore(const ITable& itable)
```
</details>


### Language-Learning Component (14 items)


**1.** `language-learning/src/grammar_learner/clustering.py:82`

**Issue:** max_clusters = min(cluster_range[1], len(vdf))  FIXME: hack 80420!

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
max([x for x in list(vdf) if isinstance(x, int)]))
#?if max([x for x in list(vdf) if isinstance(x,int)]) < cluster_range[0]+1:
#    max_clusters = min(cluster_range[1], len(vdf))  # FIXME: hack 80420!
if max([x for x in list(vdf) if isinstance(x, int)]) == 2:
return 4  # FIXME: hack Turtle 80420!
```
</details>


**2.** `language-learning/src/grammar_learner/clustering.py:84`

**Issue:** return 4  FIXME: hack Turtle 80420!

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
#    max_clusters = min(cluster_range[1], len(vdf))  # FIXME: hack 80420!
if max([x for x in list(vdf) if isinstance(x, int)]) == 2:
return 4  # FIXME: hack Turtle 80420!
n_clusters = max_clusters

```
</details>


**3.** `language-learning/src/grammar_learner/clustering.py:125`

**Issue:** n3 = mode(lst)  FIXME: Might get error?

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
if n2 != n_clusters:
if len(list(dct.values())) == len(set(list(dct.values()))):
n3 = mode(lst)  # FIXME: Might get error?
else:
n3 = n_clusters
```
</details>


**4.** `language-learning/src/grammar_learner/clustering.py:213`

**Issue:** max_clusters = 4  FIXME: hack 80420: 2D word space ⇒ 4 clusters

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
max([x for x in list(vdf) if isinstance(x, int)]))
if max([x for x in list(vdf) if isinstance(x, int)]) == 2:
max_clusters = 4  # FIXME: hack 80420: 2D word space ⇒ 4 clusters
c = pd.DataFrame(columns = ['cluster', 'cluster_words'])
s = 0
```
</details>


**5.** `language-learning/src/grammar_learner/clustering.py:232`

**Issue:** else:  check min clusters, find min viable FIXME: overkill?

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
if min_clusters > max_clusters:  # overkill?
return c, s, i
else:  # check min clusters, find min viable # FIXME: overkill?
while min_clusters < max_clusters:
try:
```
</details>


**6.** `language-learning/src/grammar_learner/grammar_inducer.py:136`

**Issue:** FIXME: add only rules with checked len(disjuncts) > 0

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
# rules['disjuncts'][cluster] = top_djs & rules['disjuncts'][cluster]
# - blocked 81205 -- might create rule without disjuncts ⇒ LG error
# FIXME: add only rules with checked len(disjuncts) > 0
i = rules['cluster'].index(cluster)
djs = top_djs & rules['disjuncts'][i]
```
</details>


**7.** `language-learning/src/grammar_learner/learner.py:186`

**Issue:** return rules, log  81126 + rules to count clusters in .ipynb tests  FIXME:DEL?

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
log.update({'grammar_learn_time': sec2string(time.time() - start)})

return rules, log  # 81126 + rules to count clusters in .ipynb tests  FIXME:DEL?


```
</details>


**8.** `language-learning/src/grammar_learner/incremental_clustering.py:357`

**Issue:** FIXME: There is an issue somewhere in tagging or filtering or input parses

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
# 190409 WSD off: [x] dct.update({word[1:-1].replace('.', '@'): label})
'''ATTN: This is still a stub result of 2 days idea check'''
# FIXME: There is an issue somewhere in tagging or filtering or input parses
#  - tagged dictionaries contain non-tagged words
#  - http://langlearn.singularitynet.io/data/clustering_2019/html/Iterative-Clustering-ILE-POCE-CDS-2019-02-28.html
```
</details>


**9.** `language-learning/src/grammar_learner/kmeans.py:61`

**Issue:** #?    max_clusters = min(cluster_range[1], len(vdf))  #FIXME: hack 80420!

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
max([x for x in list(vdf) if isinstance(x,int)]))
#?if max([x for x in list(vdf) if isinstance(x,int)]) < cluster_range[0]+1:
#?    max_clusters = min(cluster_range[1], len(vdf))  #FIXME: hack 80420!
if max([x for x in list(vdf) if isinstance(x,int)]) == 2:
# if verbose in ['max','debug']: print('2 dim word space -- 4 clusters')
```
</details>


**10.** `language-learning/src/grammar_learner/kmeans.py:65`

**Issue:** return 4  #FIXME: hack 80420!

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
# if verbose in ['max','debug']: print('2 dim word space -- 4 clusters')
logger.info('2 dim word space -- 4 clusters')
return 4  #FIXME: hack 80420!

# if verbose in ['max', 'debug']:
```
</details>


**11.** `language-learning/src/grammar_learner/kmeans.py:73`

**Issue:** #FIXME: unstable number of clusters #80422

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
n_clusters = max_clusters   #80623: cure case max < range.min

#FIXME: unstable number of clusters #80422
lst = []
attempts = 1 #12
```
</details>


**12.** `language-learning/src/grammar_learner/kmeans.py:119`

**Issue:** #this file left for POC.0.5 legacy FIXME:DEL (wait)

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
#80802 cluster_words_kmeans ⇒ clustering.py for further dev,
#number_of_clusters copied here from clustering.py,
#this file left for POC.0.5 legacy FIXME:DEL (wait)
```
</details>


**13.** `language-learning/src/grammar_learner/pqa_table.py:325`

**Issue:** linkage = clustering[0]             FIXME: all options...

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
affinity = 'euclidean'
else:
linkage = clustering[0]             # FIXME: all options...

spaces = ''
```
</details>


**14.** `language-learning/src/grammar_learner/pqa_table.py:539`

**Issue:** linkage = clustering[0]             FIXME: all options...

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
else: affinity = 'euclidean'
else:
linkage = clustering[0]             # FIXME: all options...

spaces = ''
```
</details>


### Moses Component (1 items)


**1.** `moses/moses/moses/scoring/bscores.cc:930`

**Issue:** /XXX this should probably be removed! TODO FIXME

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```
/// formula (the edges, with are printed by hand, below).
///
/// XXX this should probably be removed! TODO FIXME

cluster_bscore::cluster_bscore(const ITable& itable)
```
</details>


### Scripts Component (1 items)


**1.** `scripts/generate_todo_catalog.py:180`

**Issue:** The OpenCog Unified codebase contains distributed TODOs, FIXMEs, stubs, and "not implemented" fra...

**Category:** Distributed Systems/Research  
**Effort:** 2-6 months  
**Reasoning:** Requires distributed systems expertise or research-level work

<details>
<summary>View Code Context</summary>

```

**Problem Identification**
The OpenCog Unified codebase contains distributed TODOs, FIXMEs, stubs, and "not implemented" fragments that block full cognitive realization. These are detected by CI and halt verification (see job: https://github.com/OzCog/opencog-unified/actions/runs/16539657246/job/46779076096, ref: 25d11bfe332cd501a967d9ab3a6957a22504249f).

**Generated**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S UTC')}
```
</details>



## ⚡ Hard Priority (32 items)

### Analyze_Fixme_Instances.Py Component (3 items)


**1.** `analyze_fixme_instances.py:5`

**Issue:** This script analyzes all FIXME instances in the OpenCog Unified repository

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
FIXME Instance Analysis and Categorization Tool

This script analyzes all FIXME instances in the OpenCog Unified repository
and categorizes them by implementation difficulty based on:
- Code complexity and context
```
</details>


**2.** `analyze_fixme_instances.py:46`

**Issue:** self.fixme_instances = []

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
def __init__(self, repo_root: str):
self.repo_root = Path(repo_root)
self.fixme_instances = []
self.patterns = {
'thread_safety': r'thread[_\-\s]safe|race\s+condition|atomic|mutex|lock|concurrent|synchroniz',
```
</details>


**3.** `analyze_fixme_instances.py:126`

**Issue:** """Classify the difficulty of implementing the FIXME."""

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

def _classify_difficulty(self, instance: FIXMEInstance, patterns: Dict, full_text: str):
"""Classify the difficulty of implementing the FIXME."""

# Very Hard criteria
```
</details>


### Atomspace Component (12 items)


**1.** `atomspace/opencog/atomspace/Transient.cc:43`

**Issue:** /XXX FIXME. Performance has not been recently measuredthere

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// and ready to go. The code in this section implements this.
///
/// XXX FIXME. Performance has not been recently measured; there
/// have been a lot of redesigns since when this utility was created.
/// It is not at all clear that the code here takes less CPU/RAM than
```
</details>


**2.** `atomspace/opencog/atomspace/AtomSpace.cc:283`

**Issue:** XXX FIXME -- The recursive design of the depth() routine below makes

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

// ====================================================================
// XXX FIXME -- The recursive design of the depth() routine below makes
// it into a bottleneck, when the stack of AtomSpaces exceeds a few
// hundred. In particular, the recursion is on the C stack, and I don't
```
</details>


**3.** `atomspace/opencog/guile/SchemeEval.cc:115`

**Issue:** XXX FIXME This lock is not needed, because in guile-2.2,

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

// Lock to prevent racey setting of the output port.
// XXX FIXME This lock is not needed, because in guile-2.2,
// at least, every thread has its own output port, and so its
// impossible for two different threads to compete to set the
```
</details>


**4.** `atomspace/opencog/query/SatisfyMixin.cc:178`

**Issue:** * XXX FIXME: A major performance optimization is possible, to handle

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
* (As always, 'false' means 'search some more' and 'true' means 'halt'.
*
* XXX FIXME: A major performance optimization is possible, to handle
* the truly explosive combinatorial case. The optimization is to first
* locate all of the variables in the virtual clauses, and perform the
```
</details>


**5.** `atomspace/opencog/query/PatternMatchEngine.cc:2036`

**Issue:** /XXX FIXME: Right now, this code handles graphs that have only one

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// is not a part of the functional group.
///
/// XXX FIXME: Right now, this code handles graphs that have only one
/// single sparse search.   Nested sparse searches are not supported;
/// to implement those, its "easy": implement the same flow control as
```
</details>


**6.** `atomspace/opencog/atoms/truthvalue/FormulaTruthValue.cc:99`

**Issue:** XXX FIXME This update is not thread-safe.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME This update is not thread-safe.
void FormulaTruthValue::update(void) const
{
```
</details>


**7.** `atomspace/opencog/atoms/core/TypeUtils.cc:105`

**Issue:** "Not implemented! TODO XXX FIXME");

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
if (deep->is_unordered_link())
throw RuntimeException(TRACE_INFO,
"Not implemented! TODO XXX FIXME");

// Fall-thru and do link compares, below. Setup first.
```
</details>


**8.** `atomspace/opencog/atoms/core/TypeUtils.cc:300`

**Issue:** "Not implemented! TODO XXX FIXME");

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
if (left->is_unordered_link())
throw RuntimeException(TRACE_INFO,
"Not implemented! TODO XXX FIXME");

const HandleSeq& lout(left->getOutgoingSet());
```
</details>


**9.** `atomspace/opencog/atoms/core/TypeUtils.cc:322`

**Issue:** Interesting. XXX FIXME. This is not yet implemented!

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
ValuePtr type_compose(const Handle& left, const ValuePtr& right)
{
// Interesting. XXX FIXME. This is not yet implemented!
throw RuntimeException(TRACE_INFO, "Not implemented!");
return nullptr;
```
</details>


**10.** `atomspace/opencog/atoms/value/FormulaStream.cc:100`

**Issue:** XXX FIXME The update here is not thread-safe...

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// ==============================================================

// XXX FIXME The update here is not thread-safe...
void FormulaStream::update() const
{
```
</details>


**11.** `atomspace/opencog/atoms/flow/FilterLink.cc:211`

**Issue:** "Globbing for Values not implemented! FIXME!");

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
{
throw RuntimeException(TRACE_INFO,
"Globbing for Values not implemented! FIXME!");
}
}
```
</details>


**12.** `atomspace/opencog/scm/opencog/base/utilities.scm:1080`

**Issue:** XXX FIXME -- this is a stunningly slow and sloppy random-string

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
(seed->random-state (get-internal-real-time))))

; XXX FIXME -- this is a stunningly slow and sloppy random-string
; generator. But whatever.  I don't have the hours in the day to fix
; everything.
```
</details>


### Atomspace-Storage Component (2 items)


**1.** `atomspace-storage/opencog/persist/proxy/ProxyNode.h:75`

**Issue:** XXX FIXME Unimplemented BackingStore virtuals.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
protected:
// ----------------------------------------------------------------
// XXX FIXME Unimplemented BackingStore virtuals.
// These need to go into the assorted implementations,
// But its all very confusing and tedious, so punt.
```
</details>


**2.** `atomspace-storage/opencog/persist/sexcom/Dispatcher.h:42`

**Issue:** XXX FIXME: This is a terrible design for performance.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
{
public:
// XXX FIXME: This is a terrible design for performance.
// The std::bind call turns into seven!! stack frames of
// unwraps before the actual method is called. This is ...
```
</details>


### Cogserver Component (1 items)


**1.** `cogserver/opencog/cogserver/attic/proxy/WriteThruProxy.cc:114`

**Issue:** which seems inefficient. But still ... XXX FIXME ?

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// Sexpr::decode_slist to return a list of keys,
// and then we'd have to store one key at a time,
// which seems inefficient. But still ... XXX FIXME ?
for (const StorageNodePtr& snp : _targets)
snp->store_atom(atom);
```
</details>


### Cogutil Component (1 items)


**1.** `cogutil/opencog/util/tree.cc:9`

**Issue:** XXX FIXME This is clearly not thread-safe.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
using namespace opencog;

// XXX FIXME This is clearly not thread-safe.
// Who is using this thing ???
// Can we get rid of this file, entirely?
```
</details>


### Components Component (3 items)


**1.** `components/learning/moses/moses/comboreduct/table/table.h:98`

**Issue:** XXX FIXME TODO: change the implementation, per the above note.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// design here should be changed, so that the space-savings is still
// realized, while also allowing different types for different columns.
// XXX FIXME TODO: change the implementation, per the above note.

typedef std::vector<builtin> builtin_seq;
```
</details>


**2.** `components/learning/moses/moses/moses/scoring/scoring_base.cc:108`

**Issue:** XXX FIXME complexity_t should be a double not an int ...

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME complexity_t should be a double not an int ...
return (complexity_t) floor (cpxy / norm + 0.5);
}
```
</details>


**3.** `components/learning/moses/moses/moses/metapopulation/metapopulation.h:195`

**Issue:** /minor though in terms of performance gain. FIXME.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// scored_combo_tree_ptr_set and not having to copy and
/// reallocate candidates once they are selected. It might be
/// minor though in terms of performance gain. FIXME.
void merge_candidates(scored_combo_tree_set& candidates);

```
</details>


### Moses Component (3 items)


**1.** `moses/moses/comboreduct/table/table.h:99`

**Issue:** XXX FIXME TODO: change the implementation, per the above note.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
// design here should be changed, so that the space-savings is still
// realized, while also allowing different types for different columns.
// XXX FIXME TODO: change the implementation, per the above note.

typedef std::vector<builtin> builtin_seq;
```
</details>


**2.** `moses/moses/moses/scoring/scoring_base.cc:108`

**Issue:** XXX FIXME complexity_t should be a double not an int ...

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME complexity_t should be a double not an int ...
return (complexity_t) floor (cpxy / norm + 0.5);
}
```
</details>


**3.** `moses/moses/moses/metapopulation/metapopulation.h:195`

**Issue:** /minor though in terms of performance gain. FIXME.

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
/// scored_combo_tree_ptr_set and not having to copy and
/// reallocate candidates once they are selected. It might be
/// minor though in terms of performance gain. FIXME.
void merge_candidates(scored_combo_tree_set& candidates);

```
</details>


**4.** `atomspace/opencog/atoms/pattern/PatternTerm.h:80`

**Issue:** TODO it would probably be more efficient to swap which of these two is weak

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires performance analysis and algorithm optimization expertise

<details>
<summary>View Code Context</summary>

```
	Handle _handle;
	Handle _quote;

	// TODO: it would probably be more efficient to swap which of these
	// two is weak, since I think _outgoing is requested far more often
	// than _parent, and having it run faster would be a performance win.
	PatternTermPtr _parent;
	PatternTermWSeq _outgoing;
```
</details>


### Scripts Component (1 items)


**1.** `scripts/recursive_todo_resolver.py:200`

**Issue:** This meta-issue orchestrates the systematic resolution of TODO/FIXME items from `COMPREHENSIVE-TO...

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

## 🎯 Objective
This meta-issue orchestrates the systematic resolution of TODO/FIXME items from `COMPREHENSIVE-TODO-CATALOG.md` by priority. Each batch addresses the next {self.batch_size} unresolved, highest-priority tasks through recursive attention-allocation.

### 🧠 Cognitive Flowchart Implementation
```
</details>


### Verify_Implementations.Py Component (6 items)


**1.** `verify_implementations.py:3`

**Issue:** OpenCog Unified TODO/FIXME Implementation Verification Framework

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
#!/usr/bin/env python3
"""
OpenCog Unified TODO/FIXME Implementation Verification Framework

This framework automatically verifies that TODO/FIXME items have been properly
```
</details>


**2.** `verify_implementations.py:5`

**Issue:** This framework automatically verifies that TODO/FIXME items have been properly

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
OpenCog Unified TODO/FIXME Implementation Verification Framework

This framework automatically verifies that TODO/FIXME items have been properly
implemented and are not just placeholder code.
"""
```
</details>


**3.** `verify_implementations.py:46`

**Issue:** """Verifies that TODO/FIXME items have been properly implemented"""

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

class ImplementationVerifier:
"""Verifies that TODO/FIXME items have been properly implemented"""

def __init__(self, repo_path: str):
```
</details>


**4.** `verify_implementations.py:332`

**Issue:** OpenCog Unified TODO/FIXME Implementation Verification Report

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```
"""Generate a comprehensive verification report"""
report = f"""
# OpenCog Unified TODO/FIXME Implementation Verification Report

## Executive Summary
```
</details>


**5.** `verify_implementations.py:335`

**Issue:** - **Total TODO/FIXME items found**: {results['total_items']}

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

## Executive Summary
- **Total TODO/FIXME items found**: {results['total_items']}
- **Verified implementations**: {results['verified_implementations']}
- **Remaining TODOs**: {results['remaining_todos']}
```
</details>


**6.** `verify_implementations.py:461`

**Issue:** print("🎉 SUCCESS: All TODO/FIXME items have been properly implemented!")

**Category:** Performance/Threading/Complex Algorithm  
**Effort:** 2-8 weeks  
**Reasoning:** Requires deep technical expertise and careful implementation

<details>
<summary>View Code Context</summary>

```

if completion_rate >= 95 and tests_passed:
print("🎉 SUCCESS: All TODO/FIXME items have been properly implemented!")
return 0
elif completion_rate >= 75:
```
</details>



## 📋 Medium Priority (230 items)

### Analyze_Fixme_Instances.Py Component (29 items)


**1.** `analyze_fixme_instances.py:3`

**Issue:** FIXME Instance Analysis and Categorization Tool

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
#!/usr/bin/env python3
"""
FIXME Instance Analysis and Categorization Tool

This script analyzes all FIXME instances in the OpenCog Unified repository
```
</details>


**2.** `analyze_fixme_instances.py:28`

**Issue:** class FIXMEInstance:

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

@dataclass
class FIXMEInstance:
file_path: str
line_number: int
```
</details>


**3.** `analyze_fixme_instances.py:31`

**Issue:** fixme_text: str

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
file_path: str
line_number: int
fixme_text: str
context_lines: List[str]
difficulty: str = "UNKNOWN"
```
</details>


**4.** `analyze_fixme_instances.py:43`

**Issue:** class FIXMEAnalyzer:

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
self.dependencies = []

class FIXMEAnalyzer:
def __init__(self, repo_root: str):
self.repo_root = Path(repo_root)
```
</details>


**5.** `analyze_fixme_instances.py:58`

**Issue:** def extract_fixme_instances(self):

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

def extract_fixme_instances(self):
"""Extract all FIXME instances from source files."""
file_extensions = {'.cc', '.cpp', '.h', '.hpp', '.scm', '.py', '.c'}
```
</details>


**6.** `analyze_fixme_instances.py:59`

**Issue:** """Extract all FIXME instances from source files."""

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

def extract_fixme_instances(self):
"""Extract all FIXME instances from source files."""
file_extensions = {'.cc', '.cpp', '.h', '.hpp', '.scm', '.py', '.c'}

```
</details>


**7.** `analyze_fixme_instances.py:66`

**Issue:** print(f"Found {len(self.fixme_instances)} FIXME instances")

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
self._process_file(file_path)

print(f"Found {len(self.fixme_instances)} FIXME instances")

def _process_file(self, file_path: Path):
```
</details>


**8.** `analyze_fixme_instances.py:69`

**Issue:** """Process a single file for FIXME instances."""

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

def _process_file(self, file_path: Path):
"""Process a single file for FIXME instances."""
try:
with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
```
</details>


**9.** `analyze_fixme_instances.py:75`

**Issue:** if self._is_fixme_line(line):

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

for i, line in enumerate(lines):
if self._is_fixme_line(line):
context_start = max(0, i - 2)
context_end = min(len(lines), i + 3)
```
</details>


**10.** `analyze_fixme_instances.py:80`

**Issue:** fixme_instance = FIXMEInstance(

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
context = [l.strip() for l in lines[context_start:context_end]]

fixme_instance = FIXMEInstance(
file_path=str(file_path.relative_to(self.repo_root)),
line_number=i + 1,
```
</details>


**11.** `analyze_fixme_instances.py:83`

**Issue:** fixme_text=line.strip(),

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
file_path=str(file_path.relative_to(self.repo_root)),
line_number=i + 1,
fixme_text=line.strip(),
context_lines=context
)
```
</details>


**12.** `analyze_fixme_instances.py:87`

**Issue:** self.fixme_instances.append(fixme_instance)

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
)

self.fixme_instances.append(fixme_instance)

except Exception as e:
```
</details>


**13.** `analyze_fixme_instances.py:96`

**Issue:** 'xxx fixme' in line_lower or

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
line_lower = line.lower()
return ('fixme' in line_lower or
'xxx fixme' in line_lower or
'todo fixme' in line_lower)

```
</details>


**14.** `analyze_fixme_instances.py:97`

**Issue:** 'todo fixme' in line_lower)

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return ('fixme' in line_lower or
'xxx fixme' in line_lower or
'todo fixme' in line_lower)

def categorize_instances(self):
```
</details>


**15.** `analyze_fixme_instances.py:100`

**Issue:** """Categorize each FIXME instance by difficulty."""

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

def categorize_instances(self):
"""Categorize each FIXME instance by difficulty."""
for instance in self.fixme_instances:
self._analyze_instance(instance)
```
</details>


**16.** `analyze_fixme_instances.py:101`

**Issue:** for instance in self.fixme_instances:

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
def categorize_instances(self):
"""Categorize each FIXME instance by difficulty."""
for instance in self.fixme_instances:
self._analyze_instance(instance)

```
</details>


**17.** `analyze_fixme_instances.py:104`

**Issue:** def _analyze_instance(self, instance: FIXMEInstance):

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
self._analyze_instance(instance)

def _analyze_instance(self, instance: FIXMEInstance):
"""Analyze a single FIXME instance to determine difficulty."""
full_text = (instance.fixme_text + ' ' + ' '.join(instance.context_lines)).lower()
```
</details>


**18.** `analyze_fixme_instances.py:105`

**Issue:** """Analyze a single FIXME instance to determine difficulty."""

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

def _analyze_instance(self, instance: FIXMEInstance):
"""Analyze a single FIXME instance to determine difficulty."""
full_text = (instance.fixme_text + ' ' + ' '.join(instance.context_lines)).lower()

```
</details>


**19.** `analyze_fixme_instances.py:106`

**Issue:** full_text = (instance.fixme_text + ' ' + ' '.join(instance.context_lines)).lower()

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
def _analyze_instance(self, instance: FIXMEInstance):
"""Analyze a single FIXME instance to determine difficulty."""
full_text = (instance.fixme_text + ' ' + ' '.join(instance.context_lines)).lower()

# Count pattern matches
```
</details>


**20.** `analyze_fixme_instances.py:125`

**Issue:** def _classify_difficulty(self, instance: FIXMEInstance, patterns: Dict, full_text: str):

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
instance.reasoning = reasoning

def _classify_difficulty(self, instance: FIXMEInstance, patterns: Dict, full_text: str):
"""Classify the difficulty of implementing the FIXME."""

```
</details>


**21.** `analyze_fixme_instances.py:180`

**Issue:** """Generate a comprehensive report of FIXME instances."""

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

def generate_report(self) -> Dict:
"""Generate a comprehensive report of FIXME instances."""
# Group by difficulty
by_difficulty = defaultdict(list)
```
</details>


**22.** `analyze_fixme_instances.py:186`

**Issue:** for instance in self.fixme_instances:

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
by_file = defaultdict(list)

for instance in self.fixme_instances:
by_difficulty[instance.difficulty].append(instance)
by_category[instance.category].append(instance)
```
</details>


**23.** `analyze_fixme_instances.py:193`

**Issue:** 'total_instances': len(self.fixme_instances),

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# Create summary statistics
stats = {
'total_instances': len(self.fixme_instances),
'by_difficulty': {k: len(v) for k, v in by_difficulty.items()},
'by_category': {k: len(v) for k, v in by_category.items()},
```
</details>


**24.** `analyze_fixme_instances.py:218`

**Issue:** analyzer = FIXMEAnalyzer(repo_root)

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
"""Main analysis function."""
repo_root = "/home/runner/work/opencog-unified/opencog-unified"
analyzer = FIXMEAnalyzer(repo_root)

print("Extracting FIXME instances...")
```
</details>


**25.** `analyze_fixme_instances.py:220`

**Issue:** print("Extracting FIXME instances...")

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
analyzer = FIXMEAnalyzer(repo_root)

print("Extracting FIXME instances...")
analyzer.extract_fixme_instances()

```
</details>


**26.** `analyze_fixme_instances.py:221`

**Issue:** analyzer.extract_fixme_instances()

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

print("Extracting FIXME instances...")
analyzer.extract_fixme_instances()

print("Categorizing by difficulty...")
```
</details>


**27.** `analyze_fixme_instances.py:227`

**Issue:** report = analyzer.save_report("fixme_analysis_report.json")

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

print("Generating report...")
report = analyzer.save_report("fixme_analysis_report.json")

# Print summary
```
</details>


**28.** `analyze_fixme_instances.py:231`

**Issue:** print("FIXME ANALYSIS SUMMARY")

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# Print summary
print("\n" + "="*60)
print("FIXME ANALYSIS SUMMARY")
print("="*60)
print(f"Total FIXME instances found: {report['summary']['total_instances']}")
```
</details>


**29.** `analyze_fixme_instances.py:233`

**Issue:** print(f"Total FIXME instances found: {report['summary']['total_instances']}")

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
print("FIXME ANALYSIS SUMMARY")
print("="*60)
print(f"Total FIXME instances found: {report['summary']['total_instances']}")
print("\nBy Difficulty:")
for difficulty, count in sorted(report['summary']['by_difficulty'].items()):
```
</details>


### Atomspace Component (67 items)


**1.** `atomspace/examples/atomspace/queue.scm:7`

**Issue:** XXX FIXME, this example is not yet complete and does not yet work...

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
; to another.
;
; XXX FIXME, this example is not yet complete and does not yet work...
;
(use-modules (opencog) (opencog exec))
```
</details>


**2.** `atomspace/opencog/atomspace/AtomTable.cc:782`

**Issue:** atom in the parent. What??? XXX NOT TRUE FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// If an atom is already in the set, it will hide any duplicate
// atom in the parent. What??? XXX NOT TRUE FIXME
if (parent) {
for (const AtomSpacePtr& base : _environ)
```
</details>


**3.** `atomspace/opencog/atomspace/AtomSpace.cc:272`

**Issue:** Fixme maybe later someday, if/when this is needed.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// except that we don't have any viable mechanisms for such multiple
// membership, and so I don't know how to treat this right now.
// Fixme maybe later someday, if/when this is needed.
if (not (nullptr == _atom_space or as == nullptr))
throw RuntimeException(TRACE_INFO,
```
</details>


**4.** `atomspace/opencog/cython/PythonEval.cc:1426`

**Issue:** XXX FIXME this does a lot of wasteful string copying.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void PythonEval::eval_expr(const std::string& partial_expr)
{
// XXX FIXME this does a lot of wasteful string copying.
std::string expr = partial_expr;
size_t nl = expr.find_first_of("\n\r");
```
</details>


**5.** `atomspace/opencog/cython/PyIncludeWrapper.h:6`

**Issue:** 0.15.1 and maybe other versions)  FIXME someday...

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// XXX Cython currently conflicts with standard C library definitions.
// The push/pop below should hush it, for now. (needed for cython
// 0.15.1 and maybe other versions)  FIXME someday...
#ifdef _GNU_SOURCE
#pragma push_macro("_POSIX_C_SOURCE")
```
</details>


**6.** `atomspace/opencog/haskell/AtomSpace_CWrapper.h:112`

**Issue:** * XXX FIXME no one should be using Handle's to work with atoms,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/**
* AtomSpace_getAtomByHandle Gets an atom back from the atomspace.
* XXX FIXME no one should be using Handle's to work with atoms,
* except for the database and communications back-ends.  The
* Handle's were never intended as a user interface to atoms, and,
```
</details>


**7.** `atomspace/opencog/haskell/PatternMatcher_CWrapper.h:8`

**Issue:** * XXX FIXME: atoms must never be accessed by UUID except by the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* An interface necessary for haskell bindings.
* (ghc supports FFI for c libraries)
* XXX FIXME: atoms must never be accessed by UUID except by the
* communication and database layers. The UUID is not meant to be
* a public interface.
```
</details>


**8.** `atomspace/opencog/ocaml/CamlWrap.cc:57`

**Issue:** XXX FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
opstbl.finalize = finalize;

// XXX FIXME
opstbl.compare = custom_compare_default;
opstbl.hash = custom_hash_default;
```
</details>


**9.** `atomspace/opencog/ocaml/CamlWrap.cc:206`

**Issue:** XXX FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (vp->is_atom()) return oc_to_caml_str(HandleCast(vp), indent);

// XXX FIXME
return vp->to_short_string();
}
```
</details>


**10.** `atomspace/opencog/guile/SchemeSmobAS.cc:211`

**Issue:** * until a better permission system is invented. XXX FIXME.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
* Set the readonly flag of the atomspace.  If no atomspace specified,
* then set it on the current atomspace.  XXX This is a temporary hack,
* until a better permission system is invented. XXX FIXME.
*/
SCM SchemeSmob::ss_as_mark_readonly(SCM sas)
```
</details>


**11.** `atomspace/opencog/guile/SchemeSmobAtom.cc:84`

**Issue:** XXX FIXME. Work around the despicable, horrible guile UTF8 handling.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/* ============================================================== */

// XXX FIXME. Work around the despicable, horrible guile UTF8 handling.
// I am flabbergasted. The guile people are smart, but they could not have
// possibly picked a crappier string handling design. Fuck me. See
```
</details>


**12.** `atomspace/opencog/guile/SchemeSmobAtom.cc:210`

**Issue:** printf("XXX FIXME Bad string %s\nconverted to %s\n", (char *) data, wbuf);

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
iconv(cd, &inbuf, &ilen, &obuf, &olen);
obuf[olen] = 0x0;
printf("XXX FIXME Bad string %s\nconverted to %s\n", (char *) data, wbuf);
SCM str = scm_from_utf8_string(obuf);
free(wbuf);
```
</details>


**13.** `atomspace/opencog/guile/SchemeSmobValue.cc:242`

**Issue:** * XXX FIXME Clearly, a factory for values is called for.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/**
* Create a new value, of named type stype, and value vector svect
* XXX FIXME Clearly, a factory for values is called for.
*/
ValuePtr SchemeSmob::make_value (Type t, SCM svalue_list)
```
</details>


**14.** `atomspace/opencog/guile/SchemeEval.cc:1094`

**Issue:** XXX FIXME -- idealy we should avoid catch-and-rethrow.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Rethrow.  It would be better to just allow exceptions
// to pass on through, but thus breaks some unit tests.
// XXX FIXME -- idealy we should avoid catch-and-rethrow.
// At any rate, we must not return a TV of any sort, here.
throw RuntimeException(TRACE_INFO, "%s", _error_msg.c_str());
```
</details>


**15.** `atomspace/opencog/guile/SchemeEval.cc:1264`

**Issue:** XXX FIXME only a subset is needed.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void SchemeEval::init_scheme(void)
{
// XXX FIXME only a subset is needed.
SchemeEval sch;
}
```
</details>


**16.** `atomspace/opencog/query/InitiateSearchMixin.cc:268`

**Issue:** So we are good to go. XXX FIXME -- we could try again, to find

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// If we encountered choices, then we have enumerated all of them.
// So we are good to go. XXX FIXME -- we could try again, to find
// some thinner set of choices. Later, some other time.
if (0 < _start_choices.size()) break;
```
</details>


**17.** `atomspace/opencog/query/InitiateSearchMixin.cc:730`

**Issue:** no unit test triggers this, but its not clear why. XXX FIXME??

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// If this is used in a way such that the handle appears under a
// ChoiceLink, then an incomplete search will result!  Right now,
// no unit test triggers this, but its not clear why. XXX FIXME??
// For this case, use the `term_choices_of_handle` below.
PatternTermPtr InitiateSearchMixin::term_of_handle(const Handle& h,
```
</details>


**18.** `atomspace/opencog/query/PatternMatchEngine.h:189`

**Issue:** with it. XXX Needs to move to the Mixin class... XXX FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// State that manages the next PresentLink subterm to be grounded.
// Similar to the next-clause, above, and someday should be unified
// with it. XXX Needs to move to the Mixin class... XXX FIXME.

bool next_untried_present(const PatternTermPtr&,
```
</details>


**19.** `atomspace/opencog/query/RewriteMixin.cc:118`

**Issue:** See issue #950 and pull req #962. XXX FIXME later.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// nothing.  In practice it is difficult to insure, so
// meanwhile this try-catch is used.
// See issue #950 and pull req #962. XXX FIXME later.
// Tested by BuggyBindLinkUTest and NoExceptionUTest.
// Well, given that URE is dead meat, maybe we can remove this?
```
</details>


**20.** `atomspace/opencog/query/RewriteMixin.cc:164`

**Issue:** /XXX FIXME now I see how it can be done. The groupings should

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// clever way to find groupings in a single batch; but for now, I don't
/// see how this could be done.
/// XXX FIXME now I see how it can be done. The groupings should
/// be converted to marginals, and handled the same way. So this
/// needs a rewrite. Good thing that almost no one uses this ...
```
</details>


**21.** `atomspace/opencog/query/SatisfyMixin.cc:478`

**Issue:** XXX FIXME terrible hack.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (is_pure_absent)
{
// XXX FIXME terrible hack.
TermMatchMixin* intu =
dynamic_cast<TermMatchMixin*>(this);
```
</details>


**22.** `atomspace/opencog/query/TermMatchMixin.cc:698`

**Issue:** XXX FIXME: worse: this cannot possibly be right when

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// anything about that here? Seems like we can't do anything...
//
// XXX FIXME: worse: this cannot possibly be right when
// the ChoiceLink contains presentLinks.
for (const Handle& h : oset)
```
</details>


**23.** `atomspace/opencog/query/PatternMatchEngine.cc:283`

**Issue:** /XXX FIXME: this is currently a weak stop-gap measure to handle

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// proposed grounding. The term `ptm` points at the Present term.
///
/// XXX FIXME: this is currently a weak stop-gap measure to handle
/// the special case of Present terms embedded in Choice terms.
/// Present terms that are NOT in a Choice are handled by the
```
</details>


**24.** `atomspace/opencog/query/PatternMatchEngine.cc:1522`

**Issue:** XXX FIXME - this is not very elegant. We should probably

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// search pattern, i.e. if is is a scoped variable, then
// accept a match to any other alpha-equivalent variable.
// XXX FIXME - this is not very elegant. We should probably
// have a distinct `scoped_link_compare()` function to handle
// this. Right now, the scope_match() callback uses a rather
```
</details>


**25.** `atomspace/opencog/query/PatternMatchEngine.cc:1969`

**Issue:** XXX FIXME: Issue #3016 - Unification with unordered AndLinks

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
///
//
// XXX FIXME: Issue #3016 - Unification with unordered AndLinks
// The current implementation of unordered link permutation exploration
// in IdenticalLinks stops after finding the first valid permutation
```
</details>


**26.** `atomspace/opencog/query/PatternMatchEngine.cc:2052`

**Issue:** XXX TODO FIXME. The ptm needs to be decomposed into connected

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
logmsg("Explore sparse: Start exploration");

// XXX TODO FIXME. The ptm needs to be decomposed into connected
// components. Then only the connected components need to be walked
// over.  That would be much more efficient.
```
</details>


**27.** `atomspace/opencog/query/PatternMatchEngine.cc:2210`

**Issue:** /XXX FIXME -- do the above.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// -- build a clause_variables struct, but just for this term
/// -- search for the thinnest joint, just like `get_next_clause`
/// XXX FIXME -- do the above.
///
bool PatternMatchEngine::next_untried_present(const PatternTermPtr& parent,
```
</details>


**28.** `atomspace/opencog/guile/modules/ExecSCM.cc:73`

**Issue:** XXX FIXME: can we fix cython to not do this, already?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
// XXX FIXME: can we fix cython to not do this, already?
// Oh well. I guess that's OK, since the definition is meant to be
// for the lifetime of the process, anyway.
```
</details>


**29.** `atomspace/opencog/atoms/pattern/BindLink.cc:79`

**Issue:** Shoot. XXX FIXME. Most of the unit tests require that the atom

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
#define PLACE_RESULTS_IN_ATOMSPACE
#ifdef PLACE_RESULTS_IN_ATOMSPACE
// Shoot. XXX FIXME. Most of the unit tests require that the atom
// that we return is in the atomspace. But it would be nice if we
// could defer this indefinitely, until its really needed.
```
</details>


**30.** `atomspace/opencog/atoms/pattern/GetLink.cc:62`

**Issue:** Shoot. XXX FIXME. Most of the unit tests require that the atom

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
#define PLACE_RESULTS_IN_ATOMSPACE
#ifdef PLACE_RESULTS_IN_ATOMSPACE
// Shoot. XXX FIXME. Most of the unit tests require that the atom
// that we return is in the atomspace. But it would be nice if we
// could defer this indefinitely, until its really needed.
```
</details>


**31.** `atomspace/opencog/atoms/pattern/PatternLink.cc:146`

**Issue:** XXX FIXME, more correct would be to loop over

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// The variables for that component are just the variables
// that can be found in that component.
// XXX FIXME, more correct would be to loop over
// _pat.clause_variables and add those. Probably makes
// no difference in most cases.
```
</details>


**32.** `atomspace/opencog/atoms/pattern/PatternLink.cc:165`

**Issue:** XXX FIXME, this handles `absents`, `always` and `grouping`

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Each component consists of the assorted parts.
// XXX FIXME, this handles `absents`, `always` and `grouping`
// incorrectly.
HandleSeq clseq;
```
</details>


**33.** `atomspace/opencog/atoms/pattern/PatternLink.cc:721`

**Issue:** BUG - XXX FIXME. This extracts PresentLink's from the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
OR_LINK, SEQUENTIAL_OR_LINK, NOT_LINK});

// BUG - XXX FIXME. This extracts PresentLink's from the
// Sequentials. This is not really correct, because the
// evaluation of the sequential might terminate *before*
```
</details>


**34.** `atomspace/opencog/atoms/pattern/PatternLink.cc:1039`

**Issue:** /XXX FIXME: the code here assumes that the situation is indeed

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/// as an ordinary clause, and searched for as if it was "present".
///
/// XXX FIXME: the code here assumes that the situation is indeed
/// simple: more complex cases are not handled correctly.  Doing this
/// correctly would require iterating again, and examining the
```
</details>


**35.** `atomspace/opencog/atoms/pattern/PatternLink.cc:1255`

**Issue:** XXX FIXME -- this is wrong. What we really want is to

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
ptm->addEvaluatable();

// XXX FIXME -- this is wrong. What we really want is to
// identify those clauses that bridge across multiple
// components... not everything here does so. The
```
</details>


**36.** `atomspace/opencog/atoms/pattern/PatternLink.cc:1465`

**Issue:** XXX FIXME: debug_log() above is more readable than the below.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
DEFINE_LINK_FACTORY(PatternLink, PATTERN_LINK)

// XXX FIXME: debug_log() above is more readable than the below.
std::string PatternLink::to_long_string(const std::string& indent) const
{
```
</details>


**37.** `atomspace/opencog/atoms/truthvalue/CountTruthValue.h:73`

**Issue:** XXX FIXME Are all of these really needed?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
const MergeCtrl& mc=MergeCtrl()) const;

// XXX FIXME Are all of these really needed?
// Can we get rid of some of them?
static TruthValuePtr createTV(strength_t s, confidence_t f, count_t c)
```
</details>


**38.** `atomspace/opencog/atoms/core/TypeChoice.cc:254`

**Issue:** For now, just avoid throwing an exception. XXX FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// using a SignatureLink, but its not. As a result, it
// gets undefined behavior and incorrect results. Too bad.
// For now, just avoid throwing an exception. XXX FIXME.
return;
}
```
</details>


**39.** `atomspace/opencog/atoms/core/Checkers.cc:41`

**Issue:** XXX FIXME Much of the onfusion below is due to a bug: if the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// it does not check deep types, nor does it check arity.

// XXX FIXME Much of the onfusion below is due to a bug: if the
// types script says something like
// FOOBAR <- FUNCTION_LINK,BOOL_INPUT_LINK,NUMBER_INPUT_LINK
```
</details>


**40.** `atomspace/opencog/atoms/core/Checkers.cc:92`

**Issue:** XXX FIXME ... Perhaps IntersectionLink, UnionLink will

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// specialized operator to explicitly map the higher order into
// the lower order but as of today it is required.
// XXX FIXME ... Perhaps IntersectionLink, UnionLink will
// resolve this?
if (h->is_type(SIMILARITY_LINK) or
```
</details>


**41.** `atomspace/opencog/atoms/core/RandomChoice.cc:110`

**Issue:** XXX FIXME - fix this so it can also choose a single value

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
///

// XXX FIXME - fix this so it can also choose a single value
// out of a vector of values.
ValuePtr RandomChoiceLink::execute(AtomSpace* as, bool silent)
```
</details>


**42.** `atomspace/opencog/atoms/core/RandomChoice.cc:179`

**Issue:** XXX FIXME, also allow a FloatValue!!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
for (Handle h : ofirst->getOutgoingSet())
{
// XXX FIXME, also allow a FloatValue!!
if (h->is_executable())
h = HandleCast(h->execute(as, silent));
```
</details>


**43.** `atomspace/opencog/atoms/core/Variables.cc:109`

**Issue:** XXX FIXME URE calls us with broken handle!!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void Variables::validate_vardecl(const Handle& hdecls)
{
// XXX FIXME URE calls us with broken handle!!
if (nullptr == hdecls) return;

```
</details>


**44.** `atomspace/opencog/atoms/core/FindUtils.h:146`

**Issue:** * XXX FIXME: what if it appears quoted in one place, and unquoted

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* QuoteLink.
*
* XXX FIXME: what if it appears quoted in one place, and unquoted
* in another? then what?
*/
```
</details>


**45.** `atomspace/opencog/atoms/core/TypeUtils.cc:381`

**Issue:** returned. XXX FIXME -- surely this should be a throw, instead!!!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// If we're here we have failed to recognize vardecl as a useful
// and well-formed variable declaration, so Handle::UNDEFINED is
// returned. XXX FIXME -- surely this should be a throw, instead!!!
return Handle::UNDEFINED;
}
```
</details>


**46.** `atomspace/opencog/atoms/execution/Instantiator.cc:196`

**Issue:** /cleanly separated from each other. (XXX FIXME, these need to be

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/// walk_tree() performs a kind-of eager-evaluation of function arguments.
/// The code in here is a mashup of several different ideas that are not
/// cleanly separated from each other. (XXX FIXME, these need to be
/// cleanly separated; its impeding overall clean design/implementation.)
/// Roughly, it goes like so:
```
</details>


**47.** `atomspace/opencog/atoms/execution/Instantiator.cc:596`

**Issue:** XXX FIXME Can we defer the addition to the atomspace to an even

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// atoms to the atomspace is an expensive process.  We can save
// some time by doing it just once, right here, in one big batch.
// XXX FIXME Can we defer the addition to the atomspace to an even
// later time??
if (_as) return _as->add_atom(grounded);
```
</details>


**48.** `atomspace/opencog/atoms/execution/Instantiator.cc:640`

**Issue:** XXX FIXME, we need to get rid of this call entirely, and just

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return expr->execute(_as, silent);

// XXX FIXME, we need to get rid of this call entirely, and just
// return expr->execute(_as, silent) instead, like above.
// However, assorted parts are still broken and don't work.
```
</details>


**49.** `atomspace/opencog/atoms/join/JoinLink.cc:550`

**Issue:** /i.e. to use UnorderedHandleSet instead of HandleSet. XXX FIXME.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
///
/// TODO: it might be faster to use hash tables instead of rb-trees
/// i.e. to use UnorderedHandleSet instead of HandleSet. XXX FIXME.
HandleSet JoinLink::supremum(AtomSpace* as, bool silent,
Traverse& trav) const
```
</details>


**50.** `atomspace/opencog/atoms/join/JoinLink.cc:721`

**Issue:** XXX FIXME this is really dumb, using a queue and then

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
HandleSet hs = container(as, jcb, silent);

// XXX FIXME this is really dumb, using a queue and then
// copying things into it. Whatever. Fix this.
QueueValuePtr qvp(createQueueValue());
```
</details>


**51.** `atomspace/opencog/atoms/flow/StringOfLink.cc:120`

**Issue:** XXX FIXME this is unclear, under-specified and

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// a stream or something dynamic ... XXX ???
// Or maybe we want to cast *to* something dynamic?
// XXX FIXME this is unclear, under-specified and
// under-used at the moment.
return createStringValue(
```
</details>


**52.** `atomspace/opencog/atoms/flow/FilterLink.cc:604`

**Issue:** XXX TODO FIXME -- if vex is a stream, e.g. a QueueValue,

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
vex = _outgoing[1]->execute(as, silent);

// XXX TODO FIXME -- if vex is a stream, e.g. a QueueValue,
// then we should construct another Queue as the return value,
// and perform filtering on-demand.
```
</details>


**53.** `atomspace/opencog/atoms/flow/ValueOfLink.cc:84`

**Issue:** XXX TODO FIXME ... if either of these are executable, then

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// trickle out properly in the end.
//
// XXX TODO FIXME ... if either of these are executable, then
// they need to be executed, first, right? Yes, they do! We
// can currently get away with not doing this for two reasons:
```
</details>


**54.** `atomspace/opencog/atoms/flow/FormulaPredicateLink.cc:41`

**Issue:** /XXX FIXME - in the future, some user is going to want to include

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
/// component of the final truth value...
///
/// XXX FIXME - in the future, some user is going to want to include
/// variable declarations, and/or an explicit Lambda in the body, for
/// some reason that I cannot imagine.  The code below will then fail.
```
</details>


**55.** `atomspace/tests/query/seq-absence.scm:70`

**Issue:** ;and right now, I'm not gonna fix it... XXX FIXME.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;; Pattern matcher treats present and Absent links in a very
;; different way. This one actually does not work correctly,
;; and right now, I'm not gonna fix it... XXX FIXME.
(define and-not-present
(SatisfactionLink
```
</details>


**56.** `atomspace/tests/query/seq-absence.scm:86`

**Issue:** ;XXX FIXME ... this and the above need to get done right.

**Category:** Scheme/Logic Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** Scheme code implementation or logic system update

<details>
<summary>View Code Context</summary>

```
;; anyway, for now.  Like the above, its currently broken. Its too
;; weird right now for me to want to fix it, so I am punting on this.
;; XXX FIXME ... this and the above need to get done right.

(define or-not-absent
```
</details>


**57.** `atomspace/tests/atoms/execution/defined-schema.scm:239`

**Issue:** XXX FIXME, this does not quite work as one might naively expect,

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
; Define a recursive tree-walker. Unlike the above, this does
; not reverse the order of the edges.
; XXX FIXME, this does not quite work as one might naively expect,
; because the search results are expanded combinatorially, instead
; of being kept in branching-tree form.
```
</details>


**58.** `atomspace/examples/pattern-matcher/deduction-engine.scm:237`

**Issue:** TODO x is undefined

**Category:** Feature Implementation  
**Effort:** 1-4 weeks  
**Reasoning:** Variable scoping issue in example code that needs proper implementation

<details>
<summary>View Code Context</summary>

```
;;; Assert query 
;;;   |- likes(Bill, $y) ?
;; TODO: x is undefined
;;; This is an incorrect query. x is not defined, and so when
;;; the below is evaluated, the result is the empty set.
```
</details>


**59.** `atomspace/opencog/atomspace/AtomSpace.cc:138`

**Issue:** TODO this should probably be moved to a method on class Atom

**Category:** Code Refactoring  
**Effort:** 1-2 weeks  
**Reasoning:** Refactoring task to improve code organization

<details>
<summary>View Code Context</summary>

```

        // Check the values...
        // TODO: this should probably be moved to a method on class Atom.
        if (check_values)
        {
            for (const std::pair<const Handle, ValuePtr>& p : h->_values)
```
</details>


**60.** `atomspace/opencog/guile/SchemeEval.cc:1065`

**Issue:** TODO it would be nice to pass exceptions on through, but this currently breaks unit tests

**Category:** Error Handling  
**Effort:** 1-3 weeks  
**Reasoning:** Exception handling improvement, currently breaks unit tests

<details>
<summary>View Code Context</summary>

```
	if (_atomspace)
		SchemeSmob::ss_set_env_as(_atomspace);

	// TODO: it would be nice to pass exceptions on through, but
	// this currently breaks unit tests.
	// if (_in_eval)
	//    return scm_eval(expr, scm_interaction_environment());
```
</details>


**61.** `atomspace/opencog/atoms/core/Checkers.cc:70`

**Issue:** TODO look up the schema, and make sure its numeric, also

**Category:** Type Checking/Validation  
**Effort:** 1-3 weeks  
**Reasoning:** Schema validation and numeric type checking implementation

<details>
<summary>View Code Context</summary>

```
		// TODO - look up the schema, and make sure its numeric, also.
		if (h->is_type(NUMBER_NODE)) continue;
```
</details>


**62.** `atomspace/opencog/atoms/core/PutLink.cc:72`

**Issue:** TODO we should perform a type-check on the function

**Category:** Type Checking/Validation  
**Effort:** 1-3 weeks  
**Reasoning:** Function type validation implementation

<details>
<summary>View Code Context</summary>

```
		// XXX TODO we should perform a type-check on the function.
		// Doing this here means we'll repeat the computation many
		// times over ... which I guess is fine, for now ...
```
</details>


**63.** `atomspace/opencog/atoms/core/RandomChoice.cc:74`

**Issue:** TODO if execute() above returns FloatValue, use that!

**Category:** Feature Enhancement  
**Effort:** 1-2 weeks  
**Reasoning:** Handle FloatValue return type in execution logic

<details>
<summary>View Code Context</summary>

```
			// XXX TODO if execute() above returns FloatValue, use that!
			NumberNodePtr np(NumberNodeCast(h));
```
</details>


**64.** `atomspace/opencog/atoms/core/Variables.cc:77`

**Issue:** TODO this does not currently handle type equations, as outlined

**Category:** Type System  
**Effort:** 2-4 weeks  
**Reasoning:** Type equation handling in variable system

<details>
<summary>View Code Context</summary>

```
 * XXX TODO this does not currently handle type equations, as outlined
 * in the "math-simple.scm" demo.
 */
```
</details>


**65.** `atomspace/opencog/atoms/core/Variables.cc:158`

**Issue:** TODO type-checking could be lazy; if the function is not

**Category:** Type System Optimization  
**Effort:** 1-3 weeks  
**Reasoning:** Lazy evaluation optimization for type checking

<details>
<summary>View Code Context</summary>

```
	// XXX TODO type-checking could be lazy; if the function is not
	// evaluatable, then it could skip the type check entirely.
```
</details>


**66.** `atomspace/opencog/atoms/core/RewriteLink.cc:90`

**Issue:** TODO the following has no unit test!!! Yet it introduces a substantial behavioral change

**Category:** Testing Gap  
**Effort:** 1-2 weeks  
**Reasoning:** Add unit test for existing functionality with significant impact

<details>
<summary>View Code Context</summary>

```
		// TODO: the following has no unit test!!! Yet it introduces a
		// substantial behavioral change. It causes a reduction to be
		// performed, when before reduction was not performed.
```
</details>


**67.** `atomspace/opencog/atoms/join/JoinLink.cc:106`

**Issue:** TODO it might be faster to use hash tables instead of rb-trees

**Category:** Performance Optimization  
**Effort:** 1-3 weeks  
**Reasoning:** Data structure optimization for improved performance

<details>
<summary>View Code Context</summary>

```
/// TODO: it might be faster to use hash tables instead of rb-trees
/// i.e. to use UnorderedHandleSet instead of HandleSet. XXX FIXME.
/// But maybe not; typical join sizes might be small (???)
```
</details>


### Atomspace-Restful Component (2 items)


**1.** `atomspace-restful/opencog/python/web/api/utilities.py:17`

**Issue:** FIXME: Should this moved to the atomspace repo and be part

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# https://github.com/opencog/atomspace/pull/611
# NOTE: This is similar to scheme `cog-node`.
# FIXME: Should this moved to the atomspace repo and be part
# of opencog.atomspace module?
def get_atoms_by_name(z_type, name, atomspace):
```
</details>


**2.** `atomspace-restful/opencog/python/web/api/apiatomcollection.py:10`

**Issue:** I can't find swagger on ubuntu .. wtf!? FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
from flask_restful.utils import cors

# I can't find swagger on ubuntu .. wtf!? FIXME
from flask_restful_swagger import swagger
from opencog.bank import AttentionBank
```
</details>


### Atomspace-Rocks Component (4 items)


**1.** `atomspace-rocks/opencog/persist/monospace/MonoIO.cc:944`

**Issue:** XXX FIXME. We would like to call

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void MonoStorage::storeAtomSpace(const AtomSpace* table)
{
// XXX FIXME. We would like to call
// Options::PrepareForBulkLoad() here, but its too late, this
// can only be set when opening the DB. Should we maybe close
```
</details>


**2.** `atomspace-rocks/opencog/persist/rocks/RocksPersistSCM.cc:82`

**Issue:** XXX FIXME -- are open and close actually needed for anything?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME -- are open and close actually needed for anything?
void RocksPersistSCM::do_open(const std::string& uri)
{
```
</details>


**3.** `atomspace-rocks/opencog/persist/rocks/RocksIO.cc:697`

**Issue:** /Note: currently broken for multi-space usage, XXX FIXME.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/// Backend callback - find the Link. This is used ONLY to implement
/// the backend Query call, and is not otherwised used.
/// Note: currently broken for multi-space usage, XXX FIXME.
Handle RocksStorage::getLink(Type t, const HandleSeq& hs)
{
```
</details>


**4.** `atomspace-rocks/opencog/persist/rocks/RocksIO.cc:1286`

**Issue:** XXX FIXME. We would like to call

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
convertForFrames(HandleCast(getAtomSpace()));

// XXX FIXME. We would like to call
// Options::PrepareForBulkLoad() here, but its too late, this
// can only be set when opening the DB. Should we maybe close
```
</details>


### Atomspace-Storage Component (12 items)


**1.** `atomspace-storage/opencog/persist/proxy/ProxyNode.h:62`

**Issue:** updated at some later date. XXX FIXME.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// Virtual methods from the BackingStore API. These provide some default
// implementations. Some of these are stop-gap, and will need to be
// updated at some later date. XXX FIXME.

virtual void create(void) {} // stop-gap. FIXME
```
</details>


**2.** `atomspace-storage/opencog/persist/proxy/ProxyNode.h:64`

**Issue:** virtual void create(void) {} stop-gap. FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// updated at some later date. XXX FIXME.

virtual void create(void) {} // stop-gap. FIXME
virtual void destroy(void);  //stop-gap. FIXME
virtual void erase(void);    // stop-gap. FIXME
```
</details>


**3.** `atomspace-storage/opencog/persist/proxy/ProxyNode.h:65`

**Issue:** virtual void destroy(void) //stop-gap. FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

virtual void create(void) {} // stop-gap. FIXME
virtual void destroy(void);  //stop-gap. FIXME
virtual void erase(void);    // stop-gap. FIXME

```
</details>


**4.** `atomspace-storage/opencog/persist/proxy/ProxyNode.h:66`

**Issue:** virtual void erase(void)   stop-gap. FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
virtual void create(void) {} // stop-gap. FIXME
virtual void destroy(void);  //stop-gap. FIXME
virtual void erase(void);    // stop-gap. FIXME

virtual void proxy_open(void);
```
</details>


**5.** `atomspace-storage/opencog/persist/proxy/WriteBufferProxy.cc:195`

**Issue:** XXX FIXME. Buffering these naively, like this, voilates the

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
const ValuePtr& delta)
{
// XXX FIXME. Buffering these naively, like this, voilates the
// intent of how this method should work. However, for the
// RocksStorageNode, doing this is harmless. And the
```
</details>


**6.** `atomspace-storage/opencog/persist/proxy/ProxyNode.cc:89`

**Issue:** XXX FIXME. Using this ProxyParametersLink thing is a kind of

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// Hmm, perhaps this should be a StateLink?
//
// XXX FIXME. Using this ProxyParametersLink thing is a kind of
// cheesy hack, to pass parameters to the ProxyNode. It vaguely
// resembles the structure of an ExecutionLink, but instead of
```
</details>


**7.** `atomspace-storage/opencog/persist/proxy/ProxyNode.cc:166`

**Issue:** XXX FIXME;

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
HandleSeq ProxyNode::loadFrameDAG(void)
{
// XXX FIXME;
return HandleSeq();
}
```
</details>


**8.** `atomspace-storage/opencog/persist/sexcom/Commands.cc:165`

**Issue:** FIXME read above comment.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// This is complicated, and subject to change...
// XXX this should be nuked, and replaced by appropriate kind of proxy.
// FIXME read above comment.
std::string Commands::cog_execute_cache(const std::string& cmd)
{
```
</details>


**9.** `atomspace-storage/opencog/persist/sexcom/Commands.cc:487`

**Issue:** But still ... maybe fixme?

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
// Sexpr::decode_slist to return a list of keys, and then we'd
// have to store one key at a time, which seems inefficient.
// But still ... maybe fixme?
if (_proxy and _proxy->have_storeAtom)
_proxy->store_atom(h);
```
</details>


**10.** `atomspace-storage/opencog/persist/sexpr/ValueSexpr.cc:82`

**Issue:** * XXX FIXME This needs to be fuzzedit is very likely to crash

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* open-paren, and the string encoding the value.
*
* XXX FIXME This needs to be fuzzed; it is very likely to crash
* and/or contain bugs if it is given strings of unexpected formats.
*/
```
</details>


**11.** `atomspace-storage/opencog/persist/tlb/UuidSCM.cc:83`

**Issue:** what the heck. I'm gonna punt. XXX FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// atomspaces, and really, we should have nested hierarchical TLB
// resolvers for nested atomspaces. But for now, no one uses this so
// what the heck. I'm gonna punt. XXX FIXME.
const AtomSpacePtr& asp = SchemeSmob::ss_get_env_as("uuid");
_tlb.set_resolver(asp.get());
```
</details>


**12.** `atomspace-storage/opencog/persist/api/cython/PersistCython.cc:31`

**Issue:** XXX FIXME: except for the error messages, most of this code is

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
namespace opencog {

// XXX FIXME: except for the error messages, most of this code is
// mostly a cut-n-pate of what's in PersistSCM.cc

```
</details>


### Cogserver Component (3 items)


**1.** `cogserver/opencog/cogserver/server/CogServer.cc:125`

**Issue:** XXX FIXME. terrible terrible hack. What we should be

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
runLoopStep();

// XXX FIXME. terrible terrible hack. What we should be
// doing is running in our own thread, waiting on a semaphore,
// until some request is queued. Spinning is .. just wrong.
```
</details>


**2.** `cogserver/opencog/cogserver/server/CogServer.cc:216`

**Issue:** assumes a singleton instance, so we leave this for now. XXX FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// interesting use cases where one might want to run multiple
// cogservers. However, at this time, too much of the code (???)
// assumes a singleton instance, so we leave this for now. XXX FIXME.

// The guile module needs to be able to delete this singleton.
```
</details>


**3.** `cogserver/opencog/cogserver/server/ServerConsole.cc:261`

**Issue:** /XXX escaped quotes are not handled correctly. FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

/// Parse command line. Quotes are stripped.
/// XXX escaped quotes are not handled correctly. FIXME.
/// This passes over quotes embeded in the middle strings.
/// And that OK, because what the heck did you want to happen?
```
</details>


### Cogutil Component (10 items)


**1.** `cogutil/opencog/util/files.cc:65`

**Issue:** XXX FIXME Searching the current path is a security breach just

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
{
#ifndef WIN32
// XXX FIXME Searching the current path is a security breach just
// waiting to happen, but the current OpenCog cogserver and Config
// and unit-test-case design more or less demands this. The unit
```
</details>


**2.** `cogutil/opencog/util/Config.cc:143`

**Issue:** XXX FIXME Allowing boost to search relative paths is

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if (fin and fin.good() and fin.is_open())
{
// XXX FIXME Allowing boost to search relative paths is
// a security bug waiting to happen. Right now, it seems
// like a very very unlikely thing, but it is a bug!
```
</details>


**3.** `cogutil/opencog/util/tree.cc:47`

**Issue:** XXX THIS IS A HACK -- FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
lexeme_d[// or a message M with the syntax message:"M"
// added this to parse correctly has_said perceptions
// XXX THIS IS A HACK -- FIXME
( str_p("message:") >> ch_p('"')
>> *(anychar_p - ch_p('"')) >> ch_p('"'))
```
</details>


**4.** `cogutil/opencog/util/numeric.h:408`

**Issue:** XXX FIXME writing out the explicit loop will almost

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```

#ifdef HAVE_BOOST
// XXX FIXME writing out the explicit loop will almost
// surely be faster than calling boost. Why? Because a single
// loop allows the compiler to insert instructions into the
```
</details>


**5.** `cogutil/opencog/util/tree.h:2111`

**Issue:** FIXME: this should be optimised.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
const iterator_base& end) const
{
// FIXME: this should be optimised.
pre_order_iterator tmp=begin;
while(tmp!=end) {
```
</details>


**6.** `cogutil/opencog/util/tree.h:2680`

**Issue:** returnFIXME: we do not use first_parent_ yet, and it actually needs some serious reworking if

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void tree<T, tree_node_allocator>::fixed_depth_iterator::set_first_parent_()
{
return; // FIXME: we do not use first_parent_ yet, and it actually needs some serious reworking if
// it is ever to work at the 'head' level.
first_parent_=0;
```
</details>


**7.** `cogutil/opencog/util/tree.h:2747`

**Issue:** if(par==0) { FIXME: need to keep track of this!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
//    do {
//       par=par->next_sibling;
//       if(par==0) { // FIXME: need to keep track of this!
//          this->node=0;
//          return *this;
```
</details>


**8.** `cogutil/opencog/util/tree.h:2771`

**Issue:** if(par==0) { FIXME: need to keep track of this!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
do {
par=par->prev_sibling;
if(par==0) { // FIXME: need to keep track of this!
this->node=0;
return *this;
```
</details>


**9.** `cogutil/opencog/util/tree.h:2817`

**Issue:** FIXME: add the other members of fixed_depth_iterator.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// FIXME: add the other members of fixed_depth_iterator.


```
</details>


**10.** `cogutil/opencog/util/lazy_normal_selector.h:38`

**Issue:** XXX FIXME this makes no sensethe normal distribution is not a

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
//! number, created for testing
//
// XXX FIXME this makes no sense; the normal distribution is not a
// single number, the normal selector should return a normal
// distribution with the given mean and deviation, right?
```
</details>


### Components Component (29 items)


**1.** `components/core/atomspace-rocks/opencog/persist/monospace/MonoIO.cc:944`

**Issue:** XXX FIXME. We would like to call

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
void MonoStorage::storeAtomSpace(const AtomSpace* table)
{
// XXX FIXME. We would like to call
// Options::PrepareForBulkLoad() here, but its too late, this
// can only be set when opening the DB. Should we maybe close
```
</details>


**2.** `components/core/atomspace-rocks/opencog/persist/rocks/RocksPersistSCM.cc:82`

**Issue:** XXX FIXME -- are open and close actually needed for anything?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME -- are open and close actually needed for anything?
void RocksPersistSCM::do_open(const std::string& uri)
{
```
</details>


**3.** `components/core/atomspace-rocks/opencog/persist/rocks/RocksIO.cc:697`

**Issue:** /Note: currently broken for multi-space usage, XXX FIXME.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
/// Backend callback - find the Link. This is used ONLY to implement
/// the backend Query call, and is not otherwised used.
/// Note: currently broken for multi-space usage, XXX FIXME.
Handle RocksStorage::getLink(Type t, const HandleSeq& hs)
{
```
</details>


**4.** `components/core/atomspace-rocks/opencog/persist/rocks/RocksIO.cc:1286`

**Issue:** XXX FIXME. We would like to call

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
convertForFrames(HandleCast(getAtomSpace()));

// XXX FIXME. We would like to call
// Options::PrepareForBulkLoad() here, but its too late, this
// can only be set when opening the DB. Should we maybe close
```
</details>


**5.** `components/core/atomspace-restful/opencog/python/web/api/utilities.py:17`

**Issue:** FIXME: Should this moved to the atomspace repo and be part

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# https://github.com/opencog/atomspace/pull/611
# NOTE: This is similar to scheme `cog-node`.
# FIXME: Should this moved to the atomspace repo and be part
# of opencog.atomspace module?
def get_atoms_by_name(z_type, name, atomspace):
```
</details>


**6.** `components/core/atomspace-restful/opencog/python/web/api/apiatomcollection.py:10`

**Issue:** I can't find swagger on ubuntu .. wtf!? FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
from flask_restful.utils import cors

# I can't find swagger on ubuntu .. wtf!? FIXME
from flask_restful_swagger import swagger
from opencog.bank import AttentionBank
```
</details>


**7.** `components/learning/moses/moses/comboreduct/interpreter/eval.cc:313`

**Issue:** @todo FIXME there should be a general way to distinguish between

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
const vertex& v = *it;

// @todo FIXME there should be a general way to distinguish between
// "f" (the function f, being passed to fold) vs "f" (the function f
// being called with no arguments). If you don't handle that you get
```
</details>


**8.** `components/learning/moses/moses/comboreduct/interpreter/eval.cc:573`

**Issue:** /@todo FIXME avoid copying !?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
try {
vertex retv(eval_throws_vertex(bmap, it));
/// @todo FIXME avoid copying !?
combo_tree ret(retv);
return ret;
```
</details>


**9.** `components/learning/moses/moses/comboreduct/interpreter/eval.cc:579`

**Issue:** /@todo FIXME avoid copying !?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
catch (EvalException& e) {
vertex retv = e.get_vertex();
/// @todo FIXME avoid copying !?
combo_tree ret(retv);
return ret;
```
</details>


**10.** `components/learning/moses/moses/comboreduct/combo/vertex.h:87`

**Issue:** rand,         random contin_t in [0,1) FIXME TODO : update reduct rules

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
greater_than_zero,
impulse,      // 1.0 if true else 0.0
rand,         // random contin_t in [0,1) FIXME TODO : update reduct rules

list,           // List constructor
```
</details>


**11.** `components/learning/moses/moses/comboreduct/combo/descriptions.cc:94`

**Issue:** type. XXX FIXME...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// testing for booleans, contin as well, but not today.
// Fixing this will require work in typechecker, and use of union
// type. XXX FIXME...
{ id::equ,                  "->(enum enum boolean)" },
};
```
</details>


**12.** `components/learning/moses/moses/comboreduct/main/eval-table.cc:147`

**Issue:** XXX FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// HERE WE ARE ASSUMING THAT THE INPUT FILE HAS A HEADER!!!
// XXX FIXME
vector<string> header = get_header(pa.input_table_file);

```
</details>


**13.** `components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:620`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t bep_bscore::get_variable(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
double best_possible_precision = pos / (cnt * _true_total);
double best_possible_recall = 1.0 / _true_total;
```
</details>


**14.** `components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:629`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t bep_bscore::get_fixed(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
double best_possible_precision = pos / (cnt);
double best_possible_recall = (0.0 < pos) ? 1.0 : 0.0;
```
</details>


**15.** `components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:681`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t f_one_bscore::get_fixed(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
return 1.0;
}
```
</details>


**16.** `components/learning/moses/moses/moses/scoring/discriminating_bscore.cc:688`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t f_one_bscore::get_variable(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
double best_possible_precision = pos / cnt;
double best_possible_recall = 1.0;
```
</details>


**17.** `components/learning/moses/moses/moses/metapopulation/merging.cc:261`

**Issue:** XXX FIXME: we should use a pointer set for scored_combo_tree_set

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
candidates.size());

// XXX FIXME: we should use a pointer set for scored_combo_tree_set
// This would avoid some pointless copying here and a few other
// places.  This is easier said than done, because the stupid
```
</details>


**18.** `components/learning/moses/moses/moses/metapopulation/merging.cc:554`

**Issue:** XXX FIXME looks to me like it++ can often be collaed twice within this loop!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME looks to me like it++ can often be collaed twice within this loop!
prev_it = it++;
}
```
</details>


**19.** `components/learning/moses/moses/moses/metapopulation/ensemble.h:55`

**Issue:** * XXX FIXME: right now, the ensemble is attached to the metapop, its

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* from the metapopulation, which has no particular admission criteria.
*
* XXX FIXME: right now, the ensemble is attached to the metapop, its
* kind-of coming along for the ride, because that's easier for now.
* Someday, it should have an independent existance.
```
</details>


**20.** `components/learning/moses/moses/moses/metapopulation/metapopulation.h:90`

**Issue:** * XXX FIXME: right now, the ensemble is attached to the metapop, its

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* for inference, the metapopulation for breeding.
*
* XXX FIXME: right now, the ensemble is attached to the metapop, its
* kind-of coming along for the ride, because that's easier for now.
* Someday, it should have an independent existance.
```
</details>


**21.** `components/learning/moses/moses/moses/metapopulation/metapopulation.cc:222`

**Issue:** XXX FIXME should probably not recompute every time ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return _best_cscore;

// XXX FIXME should probably not recompute every time ...
// need to figure who is calling this method, and what they are expecting.
return _cscorer.get_cscore(_ensemble.get_ensemble());
```
</details>


**22.** `components/learning/moses/moses/moses/representation/instance_scorer.h:89`

**Issue:** XXX FIXME, calling score_tree above does not throw the exceptionthis should be done

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return _cscorer.get_cscore(tr);
} catch (...) {
// XXX FIXME, calling score_tree above does not throw the exception; this should be done
// differntly, maybe call bscorer directly, then ascorer...
// ??? Huh? why couldn't we evaluate a tree anyway?  why would we want an exception here?
```
</details>


**23.** `components/learning/moses/moses/moses/representation/build_knobs.cc:1342`

**Issue:** //FIXME: now just attaches to the first output

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

//now attach the subtree to the hidden nodes
//FIXME: now just attaches to the first output
sib_it first_hidden = it.begin();

```
</details>


**24.** `components/learning/moses/moses/moses/representation/knobs.h:204`

**Issue:** the shift is definitely in the wrong direction!! FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// WTF ??  I think the shift is n the  wrong direction, yeah?
// If the goal is to skip over index values that are disallowed, then
// the shift is definitely in the wrong direction!! FIXME.
int map_idx(int idx) const
{
```
</details>


**25.** `components/learning/moses/moses/moses/deme/deme_expander.cc:502`

**Issue:** XXX FIXME this is a bug .. the user may have specified that

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// reset scorer to use all variables (important so that
// behavioral score is consistent across generations
// XXX FIXME this is a bug .. the user may have specified that
// certain incdexes should be ignored, and this just wipes
// those out...
```
</details>


**26.** `components/learning/moses/moses/moses/main/problem-params.h:46`

**Issue:** XXX FIXME TODO The structure below should be split into multiple

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
namespace opencog { namespace moses {

// XXX FIXME TODO The structure below should be split into multiple
// parts, with each sub-part responsible for picking out the argv's
// that it cares about. Unfortunately, this requires getting rid of
```
</details>


**27.** `components/learning/moses/moses/moses/main/table-problems.cc:138`

**Issue:** XXX FIXME -- the multiple tables should be merged into one.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
logger().info("Number of rows in tables = %d", num_rows);

// XXX FIXME -- the multiple tables should be merged into one.
ctable = _ctables.front();
table = _tables.front();
```
</details>


**28.** `components/learning/moses/moses/moses/main/table-problems.cc:150`

**Issue:** XXX FIXME .. check that they all have the same signature.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Check that all input data files have the same arity
// XXX FIXME .. check that they all have the same signature.
if (_tables.size() > 1) {
for (size_t i = 1; i < _tables.size(); ++i) {
```
</details>


**29.** `components/learning/moses/moses/moses/moses/distributed_moses.cc:194`

**Issue:** Arghhh FIXME. fuser might not be installed, or it may not be in

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
bool is_being_written(const string& file_name, int pid)
{
// Arghhh FIXME. fuser might not be installed, or it may not be in
// the default search path.  RedHat/CentOS puts it into /sbin/fuser
// which is not in the default searchpath.
```
</details>


### Language-Learning Component (47 items)


**1.** `language-learning/tests/test_grammar_learner.py:26`

**Issue:** def setUp(self):    FIXME: should run before every test, but would not?!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
class TestGrammarLearner(unittest.TestCase):

def setUp(self):    # FIXME: should run before every test, but would not?!
input_parses = module_path + '/tests/data/POC-Turtle/MST_fixed_manually/'
batch_dir = module_path + '/output/Test_Grammar_Learner_' + str(UTC())[:10] + '/'
```
</details>


**2.** `language-learning/tests/test_grammar_learner.py:65`

**Issue:** 'template_path': 'poc-turtle',  FIXME: changed in June 2018 Grammar Tester

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 'test_corpus': module_path + '/data/POC-Turtle/poc-turtle-corpus.txt',
# 'reference_path': module_path + '/data/POC-Turtle/poc-turtle-parses-expected.txt',
# 'template_path': 'poc-turtle',  # FIXME: changed in June 2018 Grammar Tester
pass

```
</details>


**3.** `language-learning/tests/test_grammar_learner.py:309`

**Issue:** FIXME: check with further test_grammar updates and delete.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}
# Sometimes pqa_meter(with test_grammar updated 2018-10-19) returns pa,recall = 0,0
# FIXME: check with further test_grammar updates and delete.
x = 0.
n = 0
```
</details>


**4.** `language-learning/src/grammar_learner/hyperwords.py:27`

**Issue:** if cds != 1: sum_c = sum_c ** cds   FIXME: cds = 1.0 ?!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
sum_w = np.array(counts.sum(axis=1))[:, 0]
sum_c = np.array(counts.sum(axis=0))[0, :]
if cds != 1: sum_c = sum_c ** cds   # FIXME: cds = 1.0 ?!
sum_total = sum_c.sum()
sum_w = np.reciprocal(sum_w)
```
</details>


**5.** `language-learning/src/grammar_learner/hyperwords.py:138`

**Issue:** print('SVDEmbedding: transpose')    #FIXME:DEL

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
def __init__(self, path, normalize=True, eig=0.0, transpose=False):
if transpose:
print('SVDEmbedding: transpose')    #FIXME:DEL
ut = np.load(path + '.vt.npy')
self.wi, self.iw = load_vocabulary(path + '.contexts.vocab')
```
</details>


**6.** `language-learning/src/grammar_learner/read_files.py:123`

**Issue:** if check_mst_files(path, 'max'): FIXME: returns False ?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
else: return None
elif 'cor' in t:  # corpus: dir with file(s)
# if check_mst_files(path, 'max'): FIXME: returns False ?
if check_corpus(path):                                          # 90129
return path
```
</details>


**7.** `language-learning/src/grammar_learner/learner.py:56`

**Issue:** temp_dir = kwa('', 'temp_dir', **kwargs)  FIXME: temp_dir/tmpath  90221

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
else: corpus_stats_file = prj_dir + '/corpus_stats.txt'

temp_dir = kwa('', 'temp_dir', **kwargs)  # FIXME: temp_dir/tmpath  # 90221
tmpath = kwa('', 'tmpath', **kwargs)  # legacy
if temp_dir != '':
```
</details>


**8.** `language-learning/src/grammar_learner/learner.py:63`

**Issue:** elif tmpath == '':                              FIXME: stub!      90221

**Category:** Stub Implementation  
**Effort:** 1-3 weeks  
**Reasoning:** Replace stub/placeholder with real implementation

<details>
<summary>View Code Context</summary>

```
tmpath = temp_dir
else: tmpath = os.path.abspath(os.path.join('..')) + '/tmp'
elif tmpath == '':                              # FIXME: stub!      # 90221
# if check_dir(prj_dir + '/tmp', True, verbose):
#    tmpath = prj_dir + '/tmp'
```
</details>


**9.** `language-learning/src/grammar_learner/learner.py:80`

**Issue:** if 'error' in re01:  FIXME: assert ?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
files, re01 = check_mst_files(input_parses, verbose)
log.update(re01)
if 'error' in re01:  # FIXME: assert ?
print('learner.py » learn » check_mst_files » re01:\n', re01)
return {'error': 'input_files'}, log
```
</details>


**10.** `language-learning/src/grammar_learner/learner.py:103`

**Issue:** else:  FIXME: raise error / assert ?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
list2file(log['corpus_stats'], corpus_stats_file)
log.update({'corpus_stats_file': corpus_stats_file})
else:  # FIXME: raise error / assert ?
return {'error': 'input_files'}, log

```
</details>


**11.** `language-learning/src/grammar_learner/learner.py:116`

**Issue:** '''Generalize word categories'''  FIXME: issues with add_upper_level

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
list2file(log['corpus_stats'], corpus_stats_file)

'''Generalize word categories'''  # FIXME: issues with add_upper_level
'''
if cats_gen == 'jaccard' or (cats_gen == 'auto' and clustering == 'group'):
```
</details>


**12.** `language-learning/src/grammar_learner/learner.py:203`

**Issue:** 190221 tweak temp_dir, tmpath for Grammar Learner tutorial - FIXME line 56...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 81204-07 test and block (snooze) data pruning with max_disjuncts, etc...
# 81231 cleanup
# 190221 tweak temp_dir, tmpath for Grammar Learner tutorial - FIXME line 56...
# 190409 Optional WSD, kwargs['wsd_symbol']
# 190410 resolved empty filtered parses dataset issue
```
</details>


**13.** `language-learning/src/grammar_learner/pparser.py:88`

**Issue:** try:  FIXME: overkill? already checked by .isdigit lin3 85

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
and x[1] in tokens and x[3] in tokens:          # 190417
if x[1] == '###LEFT-WALL###': x[1] = lw             # 190424
try:  # FIXME: overkill? already checked by .isdigit lin3 85
i = int(x[0])
j = int(x[2])
```
</details>


**14.** `language-learning/src/grammar_learner/pparser.py:175`

**Issue:** terms = 'words'  legacy, not used  FIXME:DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
else:
df = mst2words(lines, **kwargs)
terms = 'words'  # legacy, not used  # FIXME:DEL?

if group:  # Always True?  # FIXME:DEL?
```
</details>


**15.** `language-learning/src/grammar_learner/pparser.py:177`

**Issue:** if group:  Always True?  FIXME:DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
terms = 'words'  # legacy, not used  # FIXME:DEL?

if group:  # Always True?  # FIXME:DEL?
df = df.groupby(['word','link'], as_index=False).sum() \
.sort_values(by=['count', 'word', 'link'],
```
</details>


**16.** `language-learning/src/grammar_learner/pparser.py:235`

**Issue:** group = True  always? » kwa(True, 'group', **kwargs)? FIXME:DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# TODO: logger = logging.getLogger(__name__ + "lines2links")
context = kwa(2, 'context', **kwargs)
group = True  # always? » kwa(True, 'group', **kwargs)? FIXME:DEL?

lines, re = filter_lines(lines, **kwargs)
```
</details>


**17.** `language-learning/src/grammar_learner/pparser.py:252`

**Issue:** ['Total  disjuncts count ', len(df)],         FIXME!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
re['corpus_stats'].extend([
['Unique disjuncts number', len(unique_djs)],
['Total  disjuncts count ', len(df)],         # FIXME!
['Average disjunct count ', avg_disjunct_count],
['Average disjunct length', avg_disjunct_length],
```
</details>


**18.** `language-learning/src/grammar_learner/pparser.py:282`

**Issue:** if group:  Always True?  FIXME:DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
['Average seed count ', avg_seed_count]])

if group:  # Always True?  # FIXME:DEL?
df = df.groupby(['word', 'link'], as_index=False).sum() \
.sort_values(by=['count', 'word', 'link'],
```
</details>


**19.** `language-learning/src/grammar_learner/sparse_word_space.py:16`

**Issue:** trash = []  81226 FIXME: return

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
max_features = kwa(1000000, 'max_features', **kwargs)  # SVS: dimension 2: disjuncts/connectors
# trash = ['.', ',', '+', '-', '?', ':', ';', '!', '"', '{', '}', '|','[', ']', '(', ')', ')(', ')(,', ', ']  # $,&,'
trash = []  # 81226 FIXME: return
stop_words = kwa(trash, 'stop_words', **kwargs)

```
</details>


**20.** `language-learning/src/grammar_learner/generalization.py:166`

**Issue:** def generalize_categories(categories, **kwargs):  80717 [F] FIXME:DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```


def generalize_categories(categories, **kwargs):  # 80717 [F] FIXME:DEL?
# categories: {'cluster':[], 'words': [], 'disjuncts':[], ...}
logger = logging.getLogger(__name__ + ".generalize_categories")
```
</details>


**21.** `language-learning/src/grammar_learner/generalization.py:415`

**Issue:** 81217 FIXME? generalize_categories [F] with new reorder (Turtle tests)

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
# 80802 fix compatibility with dj_counts & max_disjuncts, delete ...05.py?
# 81121 generalise_rules
# 81217 FIXME? generalize_categories [F] with new reorder (Turtle tests)
# 81220 refactor, test
# 81231 cleanup
```
</details>


**22.** `language-learning/src/grammar_learner/incremental_clustering.py:211`

**Issue:** rulez, re03 = learn(**kwargs)   rulez: dict FIXME: return

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

#-kwargs['output_grammar'] = kwargs['out_path']
rulez, re03 = learn(**kwargs)   # rulez: dict FIXME: return
log.update(re03)

```
</details>


**23.** `language-learning/src/grammar_learner/incremental_clustering.py:291`

**Issue:** responses = {}  FIXME: DEL or return?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

table = [['Iteration', 'N clusters', 'PA', 'F1']]
responses = {}  # FIXME: DEL or return?
np = 1000000

```
</details>


**24.** `language-learning/src/grammar_learner/incremental_clustering.py:337`

**Issue:** template_path = handle_path_string("tests/test-data/dict/poc-turtle") #FIXME:WTF?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
'\noutput_path:\n', output_path, '\n')

template_path = handle_path_string("tests/test-data/dict/poc-turtle") #FIXME:WTF?
linkage_limit = kwargs['linkage_limit'] \
if 'linkage_limit' in kwargs else 1000
```
</details>


**25.** `language-learning/src/grammar_learner/corpus_stats.py:14`

**Issue:** nlw = Counter()     non-linked words  FIXME: not used » DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
links = Counter()   # tuples: (left_word, right_word)
lw = Counter()      # linked words
nlw = Counter()     # non-linked words  # FIXME: not used » DEL?
nlws = set()        # a set of non-linked words in a sentence in a loop
nnlws = 0           # number of non-linked word occasions in all sentences
```
</details>


**26.** `language-learning/src/grammar_learner/corpus_stats.py:37`

**Issue:** nlw[sentence[j]] += 1  FIXME:DEL? nlw not returned

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
nnlws += len(nlws)  # number of non-linked words
for j in nlws:
nlw[sentence[j]] += 1  # FIXME:DEL? nlw not returned
# nlws = set()
# sentence = []
```
</details>


**27.** `language-learning/src/grammar_learner/preprocessing.py:141`

**Issue:** else:  FIXME: raise error / assert ?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
list2file(re['corpus_stats'], corpus_stats_file)
re.update({'corpus_stats_file': corpus_stats_file})
# else:  # FIXME: raise error / assert ?
#    return {'error': 'input_files'}, re

```
</details>


**28.** `language-learning/src/grammar_learner/widgets.py:142`

**Issue:** 80627 display_tree  FIXME:DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 80521 html_table, plot_2d copied from utl.turtle.py
# 80521 save file
# 80627 display_tree  FIXME:DEL?
# 80817 corpus_histograms ⇒ ? FIXME:DEL?
# 81231 cleanup
```
</details>


**29.** `language-learning/src/grammar_learner/widgets.py:143`

**Issue:** 80817 corpus_histograms ⇒ ? FIXME:DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 80521 save file
# 80627 display_tree  FIXME:DEL?
# 80817 corpus_histograms ⇒ ? FIXME:DEL?
# 81231 cleanup
```
</details>


**30.** `language-learning/src/grammar_learner/category_learner.py:32`

**Issue:** log = OrderedDict()  FIXME: log » response

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
verbose = kwa('none', 'verbose', **kwargs)

log = OrderedDict()  # FIXME: log » response
log.update({'category_learner': 'v.0.7.81231'})

```
</details>


**31.** `language-learning/src/grammar_learner/category_learner.py:53`

**Issue:** except:  FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
dim = vector_space_dim(links, dict_path, tmpath, dim_max, sv_min,
verbose)
except:  # FIXME
dim = dim_max
log.update({'vector_space_dim': dim})
```
</details>


**32.** `language-learning/src/grammar_learner/category_learner.py:167`

**Issue:** 81231 cleanup after upstream merge and conflicts resolution (FIXME: 2nd check)

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 81012 cdf2cats
# 81102 sparse wordspace agglomerative clustering
# 81231 cleanup after upstream merge and conflicts resolution (FIXME: 2nd check)
# 90221 tmpath defined in learn, tweaks removed here
# 90410 empty filtered parses dataset issue
```
</details>


**33.** `language-learning/src/grammar_learner/pqa_table.py:100`

**Issue:** template_path = handle_path_string("tests/test-data/dict/poc-turtle") FIXME:WTF?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
else kwargs['output_grammar']

template_path = handle_path_string("tests/test-data/dict/poc-turtle") # FIXME:WTF?
linkage_limit = kwargs['linkage_limit'] if 'linkage_limit' in kwargs else 1000
if 'linkage_limit' == 0:
```
</details>


**34.** `language-learning/src/grammar_learner/pqa_table.py:388`

**Issue:** Averaging ::  FIXME: stop averaging?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
kwargs['output_categories'] = oc  # = output_grammar if absent or ''

# Averaging ::  FIXME: stop averaging?
pa = []  # «parse-ability»
pq = []  # «parse quality»
```
</details>


**35.** `language-learning/src/grammar_learner/pqa_table.py:427`

**Issue:** continue  FIXME: check case

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
' ---', ' ---', ' ---', ' ---', ' ---', ' ---']
details.append(det_line)
continue  # FIXME: check case
if kwargs['linkage_limit'] > 0:
start = time.time()
```
</details>


**36.** `language-learning/src/grammar_learner/pqa_table.py:495`

**Issue:** return average, details, header, re, rulez  81120 tmp FIXME:DEL rulez?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
list2file(stats, x[:x.rfind('/')] + '/learn_&_test_stats.txt')
# return average, details, header, re
return average, details, header, re, rulez  # 81120 tmp FIXME:DEL rulez?


```
</details>


**37.** `language-learning/src/grammar_learner/pqa_table.py:498`

**Issue:** def wide_table(lines, out_dir, cp, rp, **kwargs):           81222 FIXME: [»]

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```


def wide_table(lines, out_dir, cp, rp, **kwargs):           # 81222 FIXME: [»]
# cp,rp: corpus_path, rp: reference_path for grammar tester
# runs = (1,1) (...rows) unused ⇒ FIXME:DEL from calls! [»]
```
</details>


**38.** `language-learning/src/grammar_learner/pqa_table.py:500`

**Issue:** runs = (1,1) (...rows) unused ⇒ FIXME:DEL from calls! [»]

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
def wide_table(lines, out_dir, cp, rp, **kwargs):           # 81222 FIXME: [»]
# cp,rp: corpus_path, rp: reference_path for grammar tester
# runs = (1,1) (...rows) unused ⇒ FIXME:DEL from calls! [»]
# ? module_path = os.path.abspath(os.path.join('..'))
# ? if module_path not in sys.path: sys.path.append(module_path)
```
</details>


**39.** `language-learning/src/grammar_learner/pqa_table.py:621`

**Issue:** continue  FIXME: check case

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
' ---', ' ---', ' ---', ' ---', ' ---', ' ---']
details.append(dline)
continue  # FIXME: check case
if kwargs['linkage_limit'] > 0:
start = time.time()
```
</details>


**40.** `language-learning/src/grammar_learner/pqa_table.py:667`

**Issue:** 81220 wide_table ⇒ FIXME in 2019, replace wide_row in 2019 .ipynb tests.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 81120 wide_rows
# 81210 wide_rows + min_word_count
# 81220 wide_table ⇒ FIXME in 2019, replace wide_row in 2019 .ipynb tests.
# 81231 cleanup
# 190221 tweak min_word_count (line 69)
```
</details>


**41.** `language-learning/src/grammar_learner/skl_clustering.py:110`

**Issue:** except:  FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
metrics['silhouette_index'] = float(
silhouette_score(cd, labels, metric=clustering_metric[1]))
except:  # FIXME
metrics['silhouette_index'] = 0.0
try:
```
</details>


**42.** `language-learning/src/grammar_learner/skl_clustering.py:115`

**Issue:** except:  FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
metrics['variance_ratio'] = float(
calinski_harabaz_score(cd, labels))
except:  # FIXME
metrics['variance_ratio'] = 0.0
# try:
```
</details>


**43.** `language-learning/src/grammar_learner/skl_clustering.py:123`

**Issue:** except:  else:  FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

return labels, metrics, centroids
except:  # else:  # FIXME
print('except: skl_clustering error')
return np.asarray(range(cd.shape[0])), \
```
</details>


**44.** `language-learning/src/grammar_learner/skl_clustering.py:207`

**Issue:** FIXME: try...except

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
# 190118 cleanup: remove debug printing
# 190425 fix n_clusters > n_words case
# FIXME: try...except
```
</details>


**45.** `language-learning/src/grammar_learner/write_files.py:77`

**Issue:** header = '', footer = ''):  legacy FIXME:DEL?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

def save_link_grammar(rules, output_grammar, grammar_rules = 2,
header = '', footer = ''):  # legacy FIXME:DEL?
# rules: [] or {}
# grammar_rules = kwargs['grammar_rules']: 1 ⇒ connectors, 2+ ⇒ disjuncts
```
</details>


**46.** `language-learning/src/grammar_learner/write_files.py:106`

**Issue:** line_list.sort()  FIXME: overkill?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
clusters.add(rule[0])

line_list.sort()  # FIXME: overkill?

if os.path.isfile(output_grammar):
```
</details>


**47.** `language-learning/src/grammar_learner/write_files.py:204`

**Issue:** category.append(wordz)  80704+06 tmp hack FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
wordz = deepcopy(sorted(cats['words'][i]))
#-wordz = [x.replace('@', '.') for x in wordz]  # WSD           # 190408
category.append(wordz)  # 80704+06 tmp hack FIXME
category.append(cats['similarities'][i])
# -category.append(cats['children'][i])
```
</details>


### Moses Component (23 items)


**1.** `moses/moses/comboreduct/interpreter/eval.cc:313`

**Issue:** @todo FIXME there should be a general way to distinguish between

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
const vertex& v = *it;

// @todo FIXME there should be a general way to distinguish between
// "f" (the function f, being passed to fold) vs "f" (the function f
// being called with no arguments). If you don't handle that you get
```
</details>


**2.** `moses/moses/comboreduct/interpreter/eval.cc:573`

**Issue:** /@todo FIXME avoid copying !?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
try {
vertex retv(eval_throws_vertex(bmap, it));
/// @todo FIXME avoid copying !?
combo_tree ret(retv);
return ret;
```
</details>


**3.** `moses/moses/comboreduct/interpreter/eval.cc:579`

**Issue:** /@todo FIXME avoid copying !?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
catch (EvalException& e) {
vertex retv = e.get_vertex();
/// @todo FIXME avoid copying !?
combo_tree ret(retv);
return ret;
```
</details>


**4.** `moses/moses/comboreduct/combo/vertex.h:87`

**Issue:** rand,         random contin_t in [0,1) FIXME TODO : update reduct rules

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
greater_than_zero,
impulse,      // 1.0 if true else 0.0
rand,         // random contin_t in [0,1) FIXME TODO : update reduct rules

list,           // List constructor
```
</details>


**5.** `moses/moses/comboreduct/combo/descriptions.cc:94`

**Issue:** type. XXX FIXME...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// testing for booleans, contin as well, but not today.
// Fixing this will require work in typechecker, and use of union
// type. XXX FIXME...
{ id::equ,                  "->(enum enum boolean)" },
};
```
</details>


**6.** `moses/moses/comboreduct/main/eval-table.cc:147`

**Issue:** XXX FIXME

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// HERE WE ARE ASSUMING THAT THE INPUT FILE HAS A HEADER!!!
// XXX FIXME
vector<string> header = get_header(pa.input_table_file);

```
</details>


**7.** `moses/moses/moses/scoring/discriminating_bscore.cc:620`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t bep_bscore::get_variable(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
double best_possible_precision = pos / (cnt * _true_total);
double best_possible_recall = 1.0 / _true_total;
```
</details>


**8.** `moses/moses/moses/scoring/discriminating_bscore.cc:629`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t bep_bscore::get_fixed(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
double best_possible_precision = pos / (cnt);
double best_possible_recall = (0.0 < pos) ? 1.0 : 0.0;
```
</details>


**9.** `moses/moses/moses/scoring/discriminating_bscore.cc:681`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t f_one_bscore::get_fixed(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
return 1.0;
}
```
</details>


**10.** `moses/moses/moses/scoring/discriminating_bscore.cc:688`

**Issue:** XXX TODO FIXME is this really correct?

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
score_t f_one_bscore::get_variable(score_t pos, score_t neg, unsigned cnt) const
{
// XXX TODO FIXME is this really correct?
double best_possible_precision = pos / cnt;
double best_possible_recall = 1.0;
```
</details>


**11.** `moses/moses/moses/metapopulation/merging.cc:261`

**Issue:** XXX FIXME: we should use a pointer set for scored_combo_tree_set

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
candidates.size());

// XXX FIXME: we should use a pointer set for scored_combo_tree_set
// This would avoid some pointless copying here and a few other
// places.  This is easier said than done, because the stupid
```
</details>


**12.** `moses/moses/moses/metapopulation/merging.cc:554`

**Issue:** XXX FIXME looks to me like it++ can often be collaed twice within this loop!

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

// XXX FIXME looks to me like it++ can often be collaed twice within this loop!
prev_it = it++;
}
```
</details>


**13.** `moses/moses/moses/metapopulation/ensemble.h:55`

**Issue:** * XXX FIXME: right now, the ensemble is attached to the metapop, its

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* from the metapopulation, which has no particular admission criteria.
*
* XXX FIXME: right now, the ensemble is attached to the metapop, its
* kind-of coming along for the ride, because that's easier for now.
* Someday, it should have an independent existance.
```
</details>


**14.** `moses/moses/moses/metapopulation/metapopulation.h:90`

**Issue:** * XXX FIXME: right now, the ensemble is attached to the metapop, its

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
* for inference, the metapopulation for breeding.
*
* XXX FIXME: right now, the ensemble is attached to the metapop, its
* kind-of coming along for the ride, because that's easier for now.
* Someday, it should have an independent existance.
```
</details>


**15.** `moses/moses/moses/metapopulation/metapopulation.cc:222`

**Issue:** XXX FIXME should probably not recompute every time ...

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return _best_cscore;

// XXX FIXME should probably not recompute every time ...
// need to figure who is calling this method, and what they are expecting.
return _cscorer.get_cscore(_ensemble.get_ensemble());
```
</details>


**16.** `moses/moses/moses/representation/instance_scorer.h:89`

**Issue:** XXX FIXME, calling score_tree above does not throw the exceptionthis should be done

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
return _cscorer.get_cscore(tr);
} catch (...) {
// XXX FIXME, calling score_tree above does not throw the exception; this should be done
// differntly, maybe call bscorer directly, then ascorer...
// ??? Huh? why couldn't we evaluate a tree anyway?  why would we want an exception here?
```
</details>


**17.** `moses/moses/moses/representation/build_knobs.cc:1342`

**Issue:** //FIXME: now just attaches to the first output

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

//now attach the subtree to the hidden nodes
//FIXME: now just attaches to the first output
sib_it first_hidden = it.begin();

```
</details>


**18.** `moses/moses/moses/representation/knobs.h:204`

**Issue:** the shift is definitely in the wrong direction!! FIXME.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// WTF ??  I think the shift is n the  wrong direction, yeah?
// If the goal is to skip over index values that are disallowed, then
// the shift is definitely in the wrong direction!! FIXME.
int map_idx(int idx) const
{
```
</details>


**19.** `moses/moses/moses/deme/deme_expander.cc:502`

**Issue:** XXX FIXME this is a bug .. the user may have specified that

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// reset scorer to use all variables (important so that
// behavioral score is consistent across generations
// XXX FIXME this is a bug .. the user may have specified that
// certain incdexes should be ignored, and this just wipes
// those out...
```
</details>


**20.** `moses/moses/moses/main/problem-params.h:46`

**Issue:** XXX FIXME TODO The structure below should be split into multiple

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
namespace opencog { namespace moses {

// XXX FIXME TODO The structure below should be split into multiple
// parts, with each sub-part responsible for picking out the argv's
// that it cares about. Unfortunately, this requires getting rid of
```
</details>


**21.** `moses/moses/moses/main/table-problems.cc:138`

**Issue:** XXX FIXME -- the multiple tables should be merged into one.

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
logger().info("Number of rows in tables = %d", num_rows);

// XXX FIXME -- the multiple tables should be merged into one.
ctable = _ctables.front();
table = _tables.front();
```
</details>


**22.** `moses/moses/moses/main/table-problems.cc:150`

**Issue:** XXX FIXME .. check that they all have the same signature.

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

// Check that all input data files have the same arity
// XXX FIXME .. check that they all have the same signature.
if (_tables.size() > 1) {
for (size_t i = 1; i < _tables.size(); ++i) {
```
</details>


**23.** `moses/moses/moses/moses/distributed_moses.cc:194`

**Issue:** Arghhh FIXME. fuser might not be installed, or it may not be in

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
bool is_being_written(const string& file_name, int pid)
{
// Arghhh FIXME. fuser might not be installed, or it may not be in
// the default search path.  RedHat/CentOS puts it into /sbin/fuser
// which is not in the default searchpath.
```
</details>


### Scripts Component (5 items)


**1.** `scripts/generate_todo_catalog.py:3`

**Issue:** Comprehensive TODO/FIXME Enumeration System for OpenCog Unified

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
#!/usr/bin/env python3
"""
Comprehensive TODO/FIXME Enumeration System for OpenCog Unified

This script scans the entire repository for TODO, FIXME, and related placeholders,
```
</details>


**2.** `scripts/generate_todo_catalog.py:5`

**Issue:** This script scans the entire repository for TODO, FIXME, and related placeholders,

**Category:** Feature Implementation/Algorithm  
**Effort:** 1-4 weeks  
**Reasoning:** Requires moderate technical knowledge and implementation effort

<details>
<summary>View Code Context</summary>

```
Comprehensive TODO/FIXME Enumeration System for OpenCog Unified

This script scans the entire repository for TODO, FIXME, and related placeholders,
categorizes them by subsystem, and generates a master catalog document.

```
</details>


**3.** `scripts/generate_todo_catalog.py:44`

**Issue:** Pattern to match TODO/FIXME and similar items

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
}

# Pattern to match TODO/FIXME and similar items
self.todo_patterns = [
r'TODO\s*:?\s*(.*)',
```
</details>


**4.** `scripts/generate_todo_catalog.py:47`

**Issue:** r'FIXME\s*:?\s*(.*)',

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
self.todo_patterns = [
r'TODO\s*:?\s*(.*)',
r'FIXME\s*:?\s*(.*)',
r'XXX\s*:?\s*(.*)',
r'HACK\s*:?\s*(.*)',
```
</details>


**5.** `scripts/generate_todo_catalog.py:74`

**Issue:** print(f"📊 Found {len(self.todos)} TODO/FIXME items")

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
self._scan_file(file_path)

print(f"📊 Found {len(self.todos)} TODO/FIXME items")
return self.todos

```
</details>


### Unify Component (1 items)


**1.** `unify/opencog/unify/atoms/UnifierLink.cc:141`

**Issue:** XXX FIXME, Maybe. This seems to handle all of the cases I've

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
// This is my best guess.

// XXX FIXME, Maybe. This seems to handle all of the cases I've
// looked at so far. However, the unifier has all sorts of fancy
// reduction code, and I don't understand what it is or why it
```
</details>


### Verify_Implementations.Py Component (8 items)


**1.** `verify_implementations.py:54`

**Issue:** """Scan repository for all TODO/FIXME items"""

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

def scan_repository(self) -> List[TodoItem]:
"""Scan repository for all TODO/FIXME items"""
print("🔍 Scanning repository for TODO/FIXME items...")

```
</details>


**2.** `verify_implementations.py:55`

**Issue:** print("🔍 Scanning repository for TODO/FIXME items...")

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
def scan_repository(self) -> List[TodoItem]:
"""Scan repository for all TODO/FIXME items"""
print("🔍 Scanning repository for TODO/FIXME items...")

todo_patterns = [
```
</details>


**3.** `verify_implementations.py:58`

**Issue:** r'//\s*(TODO|FIXME|XXX)[\s:]*(.+)',

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

todo_patterns = [
r'//\s*(TODO|FIXME|XXX)[\s:]*(.+)',
r'#\s*(TODO|FIXME|XXX)[\s:]*(.+)',
r';\s*(TODO|FIXME|XXX)[\s:]*(.+)',
```
</details>


**4.** `verify_implementations.py:59`

**Issue:** r'#\s*(TODO|FIXME|XXX)[\s:]*(.+)',

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
todo_patterns = [
r'//\s*(TODO|FIXME|XXX)[\s:]*(.+)',
r'#\s*(TODO|FIXME|XXX)[\s:]*(.+)',
r';\s*(TODO|FIXME|XXX)[\s:]*(.+)',
]
```
</details>


**5.** `verify_implementations.py:60`

**Issue:** r';\s*(TODO|FIXME|XXX)[\s:]*(.+)',

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
r'//\s*(TODO|FIXME|XXX)[\s:]*(.+)',
r'#\s*(TODO|FIXME|XXX)[\s:]*(.+)',
r';\s*(TODO|FIXME|XXX)[\s:]*(.+)',
]

```
</details>


**6.** `verify_implementations.py:69`

**Issue:** print(f"📊 Found {len(self.todo_items)} TODO/FIXME items")

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
self._scan_file(file_path, todo_patterns)

print(f"📊 Found {len(self.todo_items)} TODO/FIXME items")
return self.todo_items

```
</details>


**7.** `verify_implementations.py:73`

**Issue:** """Scan a single file for TODO/FIXME items"""

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```

def _scan_file(self, file_path: Path, patterns: List[str]):
"""Scan a single file for TODO/FIXME items"""
try:
with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
```
</details>


**8.** `verify_implementations.py:216`

**Issue:** return 'TODO' in line or 'FIXME' in line or 'XXX' in line

**Category:** General Implementation  
**Effort:** 1-2 weeks  
**Reasoning:** General implementation task requiring moderate effort

<details>
<summary>View Code Context</summary>

```
if item.line_number <= len(lines):
line = lines[item.line_number - 1]
return 'TODO' in line or 'FIXME' in line or 'XXX' in line
return False

```
</details>



## ✅ Easy Priority (15 items)

### Analyze_Fixme_Instances.Py Component (3 items)


**1.** `analyze_fixme_instances.py:92`

**Issue:** def _is_fixme_line(self, line: str) -> bool:

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
print(f"Error processing {file_path}: {e}")

def _is_fixme_line(self, line: str) -> bool:
"""Check if a line contains a FIXME comment."""
line_lower = line.lower()
```
</details>


**2.** `analyze_fixme_instances.py:93`

**Issue:** """Check if a line contains a FIXME comment."""

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

def _is_fixme_line(self, line: str) -> bool:
"""Check if a line contains a FIXME comment."""
line_lower = line.lower()
return ('fixme' in line_lower or
```
</details>


**3.** `analyze_fixme_instances.py:95`

**Issue:** return ('fixme' in line_lower or

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
"""Check if a line contains a FIXME comment."""
line_lower = line.lower()
return ('fixme' in line_lower or
'xxx fixme' in line_lower or
'todo fixme' in line_lower)
```
</details>


### Atomspace Component (8 items)


**1.** `atomspace/opencog/scm/opencog.scm:122`

**Issue:** FIXME: Both of the above-described problems might no longer exist.

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
; grabbing the existing atomspace, if there already is one.
;
; FIXME: Both of the above-described problems might no longer exist.
; I'm not sure. The below is simple and painless, I'm leaving it for
; now.
```
</details>


**2.** `atomspace/opencog/query/NextSearchMixin.cc:267`

**Issue:** this simpler code is good enough. XXX FIXME someday?

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
// incoming set. Ideally, we should look only at the incoming
// set that matches the type of the parent term. But for now,
// this simpler code is good enough. XXX FIXME someday?
std::multimap<std::size_t, Handle> thick_vars;

```
</details>


**3.** `atomspace/opencog/atoms/truthvalue/SimpleTruthValue.h:77`

**Issue:** XXX FIXME Are all of these really needed?

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
const MergeCtrl& mc=MergeCtrl()) const;

// XXX FIXME Are all of these really needed?
// Can we get rid of some of them?
static SimpleTruthValuePtr createSTV(strength_t mean, confidence_t conf)
```
</details>


**4.** `atomspace/opencog/atoms/flow/CollectionOfLink.cc:69`

**Issue:** FIXME: _outoging[0] could be executable, in which case

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
_have_typespec = true;

// FIXME: _outoging[0] could be executable, in which case
// is should be executed, first. But I'm lazy. Also:
// instead of being a simple type, the output could be
```
</details>


**5.** `atomspace/opencog/scm/opencog/base/atom-docs.scm:41`

**Issue:** XXX FIXME replace below by real docs.

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

; ---------------------------------------------------------------
; XXX FIXME replace below by real docs.
(set-procedure-property! BindLink 'documentation
(procedure-property QueryLink 'documentation))
```
</details>


**6.** `atomspace/opencog/cython/PythonEval.cc:24`

**Issue:** TODO When can we remove the singleton instance? Answer: not sure.

**Category:** Documentation/Design Question  
**Effort:** 1-3 days  
**Reasoning:** Documentation task to clarify singleton lifecycle

<details>
<summary>View Code Context</summary>

```
 * @todo When can we remove the singleton instance? Answer: not sure.
 * Answer: never, there are too many places in the code where this is
 * needed, and fixing those is very low priority.
```
</details>


**7.** `atomspace/opencog/atoms/core/RewriteLink.cc:90`

**Issue:** TODO the following has no unit test!!! Yet it introduces a substantial behavioral change

**Category:** Testing/Documentation  
**Effort:** 1-3 days  
**Reasoning:** Add unit test for existing functionality

<details>
<summary>View Code Context</summary>

```
		// TODO: the following has no unit test!!! Yet it introduces a
		// substantial behavioral change. It causes a reduction to be
		// performed, when before reduction was not performed.
```
</details>


**8.** `atomspace/opencog/atoms/reduct/BoolOpLink.cc:110`

**Issue:** TODO we can relax this, and accept simple truth values, too.

**Category:** Feature Relaxation  
**Effort:** 1-3 days  
**Reasoning:** Simple extension to accept additional input types

<details>
<summary>View Code Context</summary>

```
	// XXX TODO we can relax this, and accept simple truth values, too.
	// But the semantics is a bit weird... unless we really do want
	// the boolean value.
```
</details>


### Atomspace-Rocks Component (1 items)


**1.** `atomspace-rocks/opencog/persist/rocks/RocksIO.cc:505`

**Issue:** XXX FIXME (and in MonoSpace, too). According to the BackingStore

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

/// Backend callback
// XXX FIXME (and in MonoSpace, too). According to the BackingStore
// docs, it says if the Value is absent, i.e. not in storage, that
// means it has been deleted, and that it should be deleted in the
```
</details>


### Components Component (1 items)


**1.** `components/core/atomspace-rocks/opencog/persist/rocks/RocksIO.cc:505`

**Issue:** XXX FIXME (and in MonoSpace, too). According to the BackingStore

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

/// Backend callback
// XXX FIXME (and in MonoSpace, too). According to the BackingStore
// docs, it says if the Value is absent, i.e. not in storage, that
// means it has been deleted, and that it should be deleted in the
```
</details>


### Language-Learning Component (1 items)


**1.** `language-learning/tests/test_grammar_learner.py:211`

**Issue:** 81019 changes:    FIXME: DEL comments

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```
}
re = learn_grammar(**kwargs)
# 81019 changes:    # FIXME: DEL comments
# a, q, qa = pqa_meter(re['grammar_file'], outpath, cp, rp, **kwargs)
# print('parse-ability, parse-quality:', a, q)
```
</details>


### Scripts Component (1 items)


**1.** `scripts/generate_todo_catalog.py:195`

**Issue:** All TODOs, FIXMEs, and stubs are indexed, referenced by file/line, and described with actionable ...

**Category:** Documentation/Simple Fix  
**Effort:** 1-3 days  
**Reasoning:** Simple documentation updates or minor code changes

<details>
<summary>View Code Context</summary>

```

## 2. Pattern Recognition
All TODOs, FIXMEs, and stubs are indexed, referenced by file/line, and described with actionable context.
Any commit that triggers CI placeholder detection updates this master TODO issue.

```
</details>



---

## Implementation Guidance

### VERY_HARD Items - Specialized Expertise Required

**Characteristics:**
- Distributed systems algorithms
- Research-level computational problems  
- Exponential complexity algorithms
- Byzantine fault tolerance
- Consensus mechanisms

**Recommended Approach:**
- Consult academic literature and research papers
- Consider collaboration with research institutions
- Prototype and test extensively before production
- May require months of dedicated research and development

### HARD Items - Senior Developer Focus

**Characteristics:**  
- Thread safety and concurrency issues
- Performance-critical algorithms
- Complex recursive implementations
- Memory management optimizations

**Recommended Approach:**
- Assign to senior developers with domain expertise
- Implement comprehensive unit and integration tests
- Use static analysis tools (ThreadSanitizer, Valgrind)
- Code review by multiple senior developers

### MEDIUM Items - Standard Development Work

**Characteristics:**
- Feature implementations
- Algorithm completions  
- Stub replacements
- API enhancements

**Recommended Approach:**
- Can be distributed among team members
- Follow standard development workflows
- Implement with appropriate test coverage
- Regular code review process

### EASY Items - Entry-level Tasks

**Characteristics:**
- Documentation updates
- Comment improvements
- Simple code cleanups
- Minor feature additions

**Recommended Approach:**
- Suitable for new contributors
- Good for onboarding and learning codebase
- Can be completed in parallel with other work

---

## Component Analysis

| Component | VERY_HARD | HARD | MEDIUM | EASY | Total |
|-----------|-----------|------|--------|------|-------|
| analyze_fixme_instances.py | 0 | 3 | 29 | 3 | 35 |
| atomspace | 15 | 13 | 67 | 8 | 103 |
| atomspace-restful | 0 | 0 | 2 | 0 | 2 |
| atomspace-rocks | 0 | 0 | 4 | 1 | 5 |
| atomspace-storage | 2 | 2 | 12 | 0 | 16 |
| cogserver | 0 | 1 | 3 | 0 | 4 |
| cogutil | 2 | 1 | 10 | 0 | 13 |
| components | 1 | 3 | 29 | 1 | 34 |
| language-learning | 14 | 0 | 47 | 1 | 62 |
| moses | 1 | 3 | 23 | 0 | 27 |
| scripts | 1 | 1 | 5 | 1 | 8 |
| unify | 0 | 0 | 1 | 0 | 1 |
| verify_implementations.py | 0 | 6 | 8 | 0 | 14 |

---

## Next Steps

1. **Immediate Actions** (Next 2 weeks)
   - Begin with EASY items to build momentum
   - Identify domain experts for VERY_HARD items
   - Set up development environment and testing infrastructure

2. **Short Term** (1-3 months)
   - Complete all EASY items
   - Begin systematic work on MEDIUM items
   - Research and plan approach for HARD items

3. **Medium Term** (3-12 months)  
   - Complete MEDIUM and HARD items
   - Begin prototyping solutions for VERY_HARD items
   - Establish partnerships for specialized expertise

4. **Long Term** (12+ months)
   - Complete all FIXME items
   - Establish maintenance practices to prevent accumulation
   - Create contributor guidelines for high-quality code

---

*This catalog was generated automatically by analyzing 307 FIXME instances across 143 files in the OpenCog Unified repository.*
