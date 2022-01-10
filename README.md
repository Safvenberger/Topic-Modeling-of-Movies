# Topic modeling of movie plots

## Abstract
Within the entertainment industry, movies are a high grossing sector and a frequent leisure activity. Prior to watching a movie, it is often important for the consumer to know some information about it in order to choose a movie which they are interested in.  The purpose of this study is to investigate whether movies can be clustered together to generate coherent topics. By utilizing topic modeling methods Latent Dirichlet Allocation and BERTopic on movie synopses, the results show that it is possible to create coherent topics, particularly exhibited by BERTopic. The results from BERTopic also highlight that the keywords from the generated topics can be mapped to one or multiple movie genres. A (semi-) supervised approach can also be used to guide the topics toward their respective genre, but the unsupervised method also generates coherent topics.

## Data
Wikipedia movie plots: https://www.kaggle.com/jrobischon/wikipedia-movie-plots

## Methods
- Latent Dirichlet Allocation [https://radimrehurek.com/gensim/]
- BERTopic [https://github.com/MaartenGr/BERTopic]
  - Pre-trained BERT model: https://huggingface.co/sentence-transformers/all-MiniLM-L6-v2

## Notes
The main script is models.py and it should be run from the "Scripts" folder.
The models and their output are not uploaded due to size limitations.