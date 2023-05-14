---
output:
  pdf_document: default
  html_document: default
---

# Dataset

* **coordinates**: I have append y = 45 and x = 10 (highlight in red) as temporary values, when missing, to make the script/spatialization working 

* **selection**: better than a numerical threshold, should be more practical to create a new column -- called 'selection' -- where the value '1' means that the row will be analyzed.(NB: It is quite common to clean the dataset before a modeling)

* **sub-type**: when a type has a less important attribute according to me (ex: *...con bottoni laterali*, *...and "sun boat"*), I've put these values in this column. It is a suggestion to not have too many different types. But you should know if it's relevant or not

# Analysis

I've created a R computer script. So, this script is directly connected to the Google sheet. I've analyzed each period separately, but I will merge some periods as we have decided, for ex: GIIAB (550-525) + GIIAB_IIB (550-490) + GIIB (525-490). I didn't take the time to interpret the results.

Outputs for each periods are:

* 1 graph
* 2 graphs with community detection (2 different algorithms)
* 2 maps of sites with community detection (2 different algorithms)


## Graph approach

These are uniplex multi-partite graphs.

**uniplex**: one kind of edge type. This edge/relation can be described as '*has*'. For example, a site '*has*' an object

**multi-partite**: multi-classes of vertices. These classes are: Site, Object, Type, Style, etc.


### raw graphs

I plan to change the symbology of the sites' nodes depending on their context (Settlement, Funerary, ...)

* graph
  + spatialization
    - currently: Force-directed graph drawing. Choose another layout ?

* vertices (ie., nodes)
  + labels: 
    - must be shorter, create a correspondence table with numbers ?
  + color: 
    - currently: arbitrary. Choose another ?
  + size: 
    - size: currently `10` for all nodes. Choose another ? Depends on number of coonected edges ?
  + transparency:
    - currently: none. Choose another ?
    
* edges
  + width
    - currently: number of same edges
  + labels, color, size, transparency: none. Choose another ?
    
### communauty detection (CD)

Depends on the chosen algorithm, the community detection changes. I still do not understand well how they work. Veronica: maybe we can ask some leads to Fabrice Rossi ?

* algorithms
  + currently: 
    - [fastgreedy.community](https://www.rdocumentation.org/packages/igraph/versions/0.4.1/topics/fastgreedy.community)
    - [edge.betweenness.community](https://www.rdocumentation.org/packages/igraph/versions/0.4.4/topics/edge.betweenness.community)
    - others ? see: https://www.analyticsvidhya.com/blog/2020/04/community-detection-graphs-networks/
    
### graph indexes

Graph theory/Network analysis has a lot of different indexes that can be calculated to describe the differences between each graph, or between the different nodes within a graph: number of nodes, diameter, centralities, etc.