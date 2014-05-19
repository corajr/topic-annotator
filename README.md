# Topic Annotator

This Scala library can perform common preprocessing tasks on a corpus, and then run it through one of several topic models (implemented using Gibbs sampling) to produce an annotated output. It is not yet ready for general use, but should help to simplify the format wrangling needed to test different topic models on a corpus.

Check out the [org.chrisjr.topic_annotator.App](https://github.com/chrisjr/topic-annotator/blob/master/src/main/scala/org/chrisjr/topic_annotator/App.scala) class or the various [tests](https://github.com/chrisjr/topic-annotator/tree/master/src/test/scala/org/chrisjr/) for sample usage. 

Preprocessing options:
* regex tokenization
* lowercasing
* TF-IDF filtering
* stoplists

Topic models:
* [MALLET LDA](http://mallet.cs.umass.edu/topics.php)
* [HDP](https://www.github.com/arnim/HDP)
* soon, [Factorie](http://factorie.cs.umass.edu)