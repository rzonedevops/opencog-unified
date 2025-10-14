# language-learning/src/grammar_inducer.py                              # 81207
import logging
from copy import deepcopy
from collections import Counter
from typing import List, Tuple
from .utl import UTC, kwa


def add_disjuncts(cats, links, **kwargs):
    # add disjuncts to {cats} after k-means or agglomerative clustering
    # cats: {'cluster': [], 'words': [], }
    # Add disjuncts to categories after clustering
    # Note: Pruning of disjuncts is handled separately in prune_cats()
    max_disjuncts = kwa(100000, 'max_disjuncts', **kwargs)
    verbose = kwa('none', 'verbose', **kwargs)

    fat_cats = deepcopy(cats)
    top_clusters = [i for i, x in enumerate(cats['cluster']) if
                    i > 0 and x is not None]
    word_clusters = dict()
    for i in top_clusters:
        for word in cats['words'][i]:
            word_clusters[word] = i

    df = links.copy()
    df['cluster'] = df['word'].apply(
        lambda x: word_clusters[x] if x in word_clusters else 0)
    cdf = df.groupby('cluster').sum().reset_index()
    cdf = cdf.loc[cdf['cluster'] > 0]
    fat_cats['counts'] = [0] + cdf['count'].tolist()

    fat_cats['disjuncts'] = [[]]
    fat_cats['dj_counts'] = [[]]
    cdf = df.groupby(['cluster', 'link'], as_index = False).sum() \
        .sort_values(by = ['cluster', 'count'], ascending = [True, False])

    # Note: Disjunct counting is implemented in induce_grammar() for better performance

    for cluster in top_clusters:
        ccdf = cdf.loc[cdf['cluster'] == cluster]
        fat_cats['disjuncts'].append(ccdf['link'].tolist())
        fat_cats['dj_counts'].append(ccdf['count'].tolist())

    fat_cats['djs'] = [[]]
    ldf = df[['link', 'count']].copy().groupby('link').sum().sort_values(
        by = 'count', ascending = False).reset_index()
    djdict = {x: i for i, x in enumerate(ldf['link'].tolist())}
    df.drop(['word'], axis = 1, inplace = True)
    df['dj'] = df['link'].apply(lambda x: djdict[x])
    cdf = df.groupby(['cluster', 'dj'], as_index = False).sum().sort_values(
        by = ['cluster', 'dj'], ascending = [True, True])
    for cluster in top_clusters:
        ccdf = cdf.loc[cdf['cluster'] == cluster]
        fat_cats['djs'].append(ccdf['dj'].tolist())

    return fat_cats


def prune_cats(categories, **kwargs):  # 81204 checked as check_cats ~OK?
    # check each category has associated disjuncts, delete if no disjuncts
    # 81204 ad-hoc:  lost hierarchy, connectors to deleted clusters :(
    cats = {key: [] for key in categories.keys()}
    for i, djs in enumerate(cats['disjuncts']):
        if len(djs) > 0:
            for key in cats.keys():
                cats[key].append(categories[key][i])
        else:
            continue
    return cats, {'prune_cats': 'under construction'}


def check_cats(cats, **kwargs):
    orphans = []
    for i, djs in enumerate(cats['disjuncts']):
        if len(djs) == 0:
            orphans.append([i, cats['cluster'][i], cats['words'][i], djs])
    if len(orphans) > 0:
        print('check_cats » orphans:', orphans)
    return {'orphaned_clusters': orphans}


def induce_grammar(categories, **kwargs):
    logger = logging.getLogger(__name__ + ".induce_grammar")
    # categories: {'cluster': [], 'words': [], ...}
    max_disjuncts = kwa(100000, 'max_disjuncts', **kwargs)
    verbose = kwa('none', 'verbose', **kwargs)

    rules = deepcopy(categories)
    dj_counts = Counter()
    clusters = [i for i, x in enumerate(rules['cluster'])
                if i > 0 and x is not None]
    word_clusters = dict()
    for i in clusters:
        for word in rules['words'][i]:
            word_clusters[word] = i
    disjuncts = {}

    for cluster in clusters:
        djs = []
        for i, rule in enumerate(categories['disjuncts'][cluster]):
            if type(rule) is str:  # 'a- & was-' ⇒ (-9,-26) + reverse 81012
                x = rule.split()
                lefts = []
                rights = []
                for y in x:
                    if (y not in ['&', ' ', '']) and (y[:-1] in word_clusters):
                        if y[-1] == '+':
                            rights.append(word_clusters[y[:-1]])
                        elif y[-1] == '-':
                            lefts.append(-1 * word_clusters[y[:-1]])
                        else:
                            logger.warning(f'No sign found in rule "{y}" in "{x}" - skipping')
                            continue
                lefts.reverse()  # 81012
                dj = lefts + rights
                if len(dj) > 0:
                    djs.append(tuple(dj))
                    dj_counts[tuple(dj)] += categories['dj_counts'][cluster][i]

        rules['disjuncts'][cluster] = set(djs)

    # Prune rules: Keep only top-frequency disjuncts and remove empty rules
    top_djs = set([x[0] for x in dj_counts.most_common(max_disjuncts)])
    
    # Create a new rules dict with only valid rules (those with disjuncts)
    pruned_rules = {key: [] for key in rules.keys()}
    pruned_clusters = []
    pruned_words = []
    
    # Always keep the first element (index 0) which is typically empty/null
    for key in pruned_rules.keys():
        pruned_rules[key].append(rules[key][0] if len(rules[key]) > 0 else None)
    
    # Process all non-null clusters
    clusters = [x for i, x in enumerate(rules['cluster'])
                if i > 0 and x is not None]
    
    for cluster in clusters:
        i = rules['cluster'].index(cluster)
        djs = top_djs & rules['disjuncts'][i]
        
        # Only keep rules that have at least one disjunct after pruning
        if len(djs) > 0:
            for key in pruned_rules.keys():
                if key == 'disjuncts':
                    pruned_rules[key].append(djs)
                else:
                    pruned_rules[key].append(rules[key][i] if i < len(rules[key]) else None)
        else:
            # Track pruned clusters for logging
            pruned_clusters.append(cluster)
            pruned_words.append(rules['words'][i] if i < len(rules['words']) else [])
            logger.debug(f"Pruned cluster {cluster} with words {rules['words'][i]} - no valid disjuncts")
    
    # Update rules with pruned version
    rules = pruned_rules
    
    if len(pruned_clusters) > 0:
        logger.info(f"Pruned {len(pruned_clusters)} clusters with no valid disjuncts")

    return rules, {
        'learned_rules': len([x for i, x in enumerate(rules['parent'])
                              if x == 0 and i > 0]),
        'total_clusters': len(rules['cluster']) - 1
    }

# Notes:

# 80802 poc05.py restructured: induce_grammar ⇒ grammar_inducer.py v~80625
# 81102 max_disjuncts ⇒ induce_grammar
# 81204 add_disjuncts, check_cats, prune_cats :: resolve rules with empty dj list
# 81231 cleanup
