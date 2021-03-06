# Recommender Systems

<img width = "600" src="https://github.com/victoria-yang/recommender-systems/blob/master/Int_dif.png">

### Description
In HUDK4051 there are six units, this recommender system will use the student's response in a survey, which includes ratings of these units in terms of both interest and difficulty to produce individual suggestions about what unit to attempt next.


### Prerequisites

**R Packages**

```
install.packages("tidyr")
install.packages("lsa")
install.packages("corrplot")
```

## Datasets Info
 **difficulty.csv**

 	This dataset contains the ratings of ecpected difficulty for each topic.

 	* Variables:
 		- name
 		- pred.dif
 		- nlp.dif
 		- sna.dif
 		- neural.dif
 		- viz.dif
 		- loop.dif

 **interest.csv**

 	This dataset contains the ratings of student's interests for each topic.

 	* Variables:
 		- name
 		- pred.int
 		- nlp.int
 		- sna.int
 		- neural.int
 		- viz.int
 		- loop.int


## Procedure

**Building collaborative filter**

* Generate a user-based similarity matrix for interest data based on cosine similarity using the ratings the class gave each unit.

* Find out which students are most similar to you.

* Then create a unit-based, rather than student-based similarity matrix for difficulty

* Create a composite measure from interest and difficulty (Principal component analysis )





## Author
[Chieh-An (Victoria) Yang](https://www.linkedin.com/in/victoria-chieh-an-yang/) - Learning Analytics MS student at Teachers College, Columbia University

