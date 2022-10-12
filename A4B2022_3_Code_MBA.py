# 1.Libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from mlxtend.frequent_patterns import apriori
from mlxtend.frequent_patterns import association_rules
import networkx as nx


# 2.Datasets
# Change the path of your directories below to be able to import the datasets.
products = pd.read_csv('C:/Users/aubin/OneDrive/Documents/Cours/AFB/MBA_datasets/MBA_datasets/products_DB.csv',
                       sep=';', encoding='latin-1')
groups = pd.read_csv('C:/Users/aubin/OneDrive/Documents/Cours/AFB/MBA_datasets/MBA_datasets/products_DB_groups.csv',
                     sep=';', encoding='latin-1')
ticket = pd.read_csv(
    'C:/Users/aubin/OneDrive/Documents/Cours/AFB/MBA_datasets/MBA_datasets/ticket_description.csv', sep=',')


# 3.Data exploration
# Get an overview of our datasets architecture:
print(products.head())
print(ticket.head())
print(groups.head())
# Detect null values in our datasets. In our case it's False:
print(products.isnull().values.any())
print(ticket.isnull().values.any())
# Display information about our datasets:
print(ticket.info())
print(products.info())
print(products.info())


# 4.Preprocessing

# a. Function used in preprocessing - item encoding:
def encode_units(x):
    if x <= 0:
        return 0
    if x >= 1:
        return 1


# b. Preprocessing: arguments = [products, groups, ticket datasets],
# and returns both basket sets for grouped and non grouped items :


def preprocessing(products, groups, ticket):
    # Merge the datasets and add the column Quantity for grouped items:
    df_group = pd.merge(groups, ticket, on='prod_id', how='inner')
    df_group.insert(3, "Quantity", 1, True)
    # Idem for non grouped items:
    df = pd.merge(products, ticket, on='prod_id', how='inner')
    df.insert(3, "Quantity", 1, True)

    # Grouped items encoding - to show if an item is present or not on a ticket:
    basket_groups = (df_group.groupby(['ticket_id', 'Group'])[
                     'Quantity'].sum().unstack().reset_index().fillna(0).set_index('ticket_id'))
    # If an item in purchased several times on the same ticket, it returns 1 using the encode_units() function.
    basket_sets_groups = basket_groups.applymap(encode_units)
    basket_sets_groups = basket_sets_groups[(
        basket_sets_groups > 0).sum(axis=1) >= 2]

    # Idem for non grouped items:
    basket = (df.groupby(['ticket_id', 'Description'])['Quantity'].sum(
    ).unstack().reset_index().fillna(0).set_index('ticket_id'))
    basket_sets = basket.applymap(encode_units)
    basket_sets = basket_sets[(basket_sets > 0).sum(axis=1) >= 2]

    return basket_sets_groups, basket_sets


# 5.Data transformation using Apriori:

# Training apriori model trough apriori_computation function:
def apriori_computation(basket_sets_groups, basket_sets):

    # Training apriori for grouped items:
    # We decide to filter support to be higher than 0.0005 to have significative products.
    # and we sort values by descending support values:
    frequent_itemsets_groups = apriori(basket_sets_groups, min_support=0.0005, max_len=2,
                                       use_colnames=True).sort_values('support', ascending=False).reset_index(drop=True)
    frequent_itemsets_groups['lenght'] = frequent_itemsets_groups['itemsets'].apply(
        lambda x: len(x))

    # Idem for non grouped items:
    frequent_itemsets = apriori(basket_sets, min_support=0.00005, max_len=3,
                                use_colnames=True).sort_values('support', ascending=False).reset_index(drop=True)
    frequent_itemsets['lenght'] = frequent_itemsets['itemsets'].apply(
        lambda x: len(x))

    return(frequent_itemsets_groups, frequent_itemsets)


# 6.Initialisation
basket_sets_groups, basket_sets_items = preprocessing(products, groups, ticket)

frequent_itemsets_groups, frequent_itemsets = apriori_computation(
    basket_sets_groups, basket_sets_items)


# 7. Rules computations for groups


# Groups sorted by lift with only high support of antecedents
rules_groups = association_rules(frequent_itemsets_groups, metric="lift", min_threshold=1).sort_values(
    'lift', ascending=False).reset_index(drop=True)
filtered_rules_groups = (rules_groups[(rules_groups['antecedent support'] > 0.05) &
                                      (rules_groups['consequent support'] > 0.05) &
                                      (rules_groups['confidence'] > 0.3)]).sort_values('lift', ascending=False).reset_index(drop=True)
print(filtered_rules_groups)

# Groups sorted by lift with low support
rules_groups = association_rules(frequent_itemsets_groups, metric="lift", min_threshold=1).sort_values(
    'lift', ascending=False).reset_index(drop=True)
filtered_rules_groups = (rules_groups[(rules_groups['antecedent support'] < 0.02) &
                                      (rules_groups['consequent support'] > 0.0) &
                                      (rules_groups['confidence'] > 0.1)]).sort_values('lift', ascending=False).reset_index(drop=True).head(10)
print(filtered_rules_groups)

# Sorted by sun cream
rules_groups = association_rules(frequent_itemsets_groups, metric="lift", min_threshold=0).sort_values(
    'lift', ascending=False).reset_index(drop=True)
filtered_rules_groups = (rules_groups[(rules_groups['antecedent support'] > 0.00) &
                                      (rules_groups['consequent support'] > 0.00) &
                                      (rules_groups['antecedents'] == frozenset({'Sun cream'})	) &
                                      (rules_groups['confidence'] > 0.1)]).sort_values('lift', ascending=False).reset_index(drop=True)
print(filtered_rules_groups)

# Sorted by garden products
rules_groups = association_rules(frequent_itemsets_groups, metric="lift", min_threshold=0).sort_values(
    'lift', ascending=False).reset_index(drop=True)
filtered_rules_groups = (rules_groups[(rules_groups['antecedent support'] > 0.00) &
                                      (rules_groups['consequent support'] > 0.00) &
                                      (rules_groups['antecedents'] == frozenset({'Garden products'})	) &
                                      (rules_groups['confidence'] > 0.1)]).sort_values('lift', ascending=False).reset_index(drop=True).head(20)
print(filtered_rules_groups)

# Sorted by vegetarian/vegan products
rules_groups = association_rules(frequent_itemsets_groups, metric="lift", min_threshold=0).sort_values(
    'lift', ascending=False).reset_index(drop=True)
filtered_rules_groups = (rules_groups[(rules_groups['antecedent support'] > 0.00) &
                                      (rules_groups['consequent support'] > 0.00) &
                                      (rules_groups['antecedents'] == frozenset({'Vegeterian/vegan food'})	) &
                                      (rules_groups['confidence'] > 0.1)]).sort_values('lift', ascending=False).reset_index(drop=True).head(20)
print(filtered_rules_groups)

# Sorted by baby products
rules_groups = association_rules(frequent_itemsets_groups, metric="lift", min_threshold=0).sort_values(
    'lift', ascending=False).reset_index(drop=True)
filtered_rules_groups = (rules_groups[(rules_groups['antecedent support'] > 0.00) &
                                      (rules_groups['consequent support'] > 0.00) &
                                      (rules_groups['antecedents'] == frozenset({'Baby products'})	) &
                                      (rules_groups['confidence'] > 0.1)]).sort_values('lift', ascending=False).reset_index(drop=True).head(20)
print(filtered_rules_groups)


# 8. Rules computations for items


# high support and high confidence specific products
rules = association_rules(frequent_itemsets, metric="lift", min_threshold=1).sort_values(
    'lift', ascending=False).reset_index(drop=True)
filtered_rules = (rules[(rules['antecedent support'] > 0.02) &
                        (rules['consequent support'] > 0.02) &
                        (rules['confidence'] > 0.1)]).sort_values('lift', ascending=False).reset_index(drop=True).head(20)
print(filtered_rules)

# Highest lifts with confidence >0.1 of specific products
rules = association_rules(frequent_itemsets, metric="lift", min_threshold=1).sort_values(
    'lift', ascending=False).reset_index(drop=True)
filtered_rules = (rules[(rules['antecedent support'] > 0.0) &
                        (rules['consequent support'] > 0.0) &
                        (rules['confidence'] > 0.1)]).sort_values('lift', ascending=False).reset_index(drop=True).head(20)
print(filtered_rules)



# 9. Other interesting rules but not used in the final analysis because less relevant


# Sorted by Pasta item
rules = association_rules(frequent_itemsets, metric="lift", min_threshold=0).sort_values(
    'lift', ascending=False).reset_index(drop=True)
filtered_rules = (rules[(rules['antecedent support'] > 0.002) &
                        (rules['consequent support'] > 0.002) &
                        (rules['antecedents'] == frozenset({'Pasta'})	) &
                        (rules['confidence'] > 0.1)]).sort_values('lift', ascending=False).reset_index(drop=True).head(20)
print(filtered_rules)

# Sorted by Cosmetics group
rules_groups = association_rules(frequent_itemsets_groups, metric="lift", min_threshold=0).sort_values(
    'lift', ascending=False).reset_index(drop=True)
filtered_rules_groups = (rules_groups[(rules_groups['antecedent support'] > 0.00) &
                                      (rules_groups['consequent support'] > 0.00) &
                                      (rules_groups['antecedents'] == frozenset({'Cosmetics'})	) &
                                      (rules_groups['confidence'] > 0.30)]).sort_values('lift', ascending=False).reset_index(drop=True)
print(filtered_rules_groups)