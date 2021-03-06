# -*- coding: utf-8 -*-
"""
Created on Tue Dec 14 07:48:19 2021

@author: Rasmus Säfvenberg
"""
import pandas as pd
import numpy as np

# Helper functions
from preprocessing import process_text, convert_to_int, lda_preprocess
from genre_processing import clean_genre, group_genre
from pprint import pprint

# BERTopic
from sentence_transformers import SentenceTransformer
from bertopic import BERTopic
from umap import UMAP

# LDA imports
from gensim.corpora import Dictionary
from gensim.models import LdaModel, Phrases, CoherenceModel
import logging


# Define classes
class lda_model:
    def __init__(self, nr_topics=20, nr_passes=50):
        self.nr_topics = nr_topics
        self.nr_passes = nr_passes
        self.is_fitted = False
        
    def _create_lda_documents(docs):
        """
        Convert the documents to a structure that aligns with the LdaModel.

        Parameters
        ----------
        docs : pd.Series, np.array or similar one dimensional structure
            Contains the synopses of all the movies in the training data.

        Returns
        -------
        documents : list of lists
            A structure to work with the LdaModel from gensim.

        """
        # Preprocess the documents to work with the LDA model
        documents = lda_preprocess(docs)
        
        # Create bigrams
        bigram = Phrases(documents)
        for idx in range(len(documents)):
            for token in bigram[documents[idx]]:
                if '_' in token:
                    # Token is a bigram, add to document.
                    documents[idx].append(token)
        
        return documents
        
    
    def _convert_lda_to_df(self, model_preds):
        """
        Save the predicted probabilites for each document to belong to a topic
        in a data frame.

        Parameters
        ----------
        model_preds : list of tuples
            Structure as generated by predicting topics for new documents.

        Returns
        -------
        lda_df : pd.DataFrame
            A data frame with all possible topic predictions and each documents
            probability of being in that topic.

        """
        lda_df = pd.DataFrame(columns=range(self.nr_topics))
        for doc in model_preds:
            # Convert list of tuple to a dataframe
            a = pd.DataFrame.from_dict({x[0]:x[1] for x in doc}, orient="index").T
            
            # Add extra columns so that it contains all topics
            a = a.reindex(columns=range(self.nr_topics), fill_value=0)
            
            # Append to the dataframe
            lda_df = lda_df.append(a)
        
        lda_df.reset_index(inplace=True, drop=True)
        
        return lda_df
    
    
    def create_dictionary(self, X):
        """
        Create the dicationary, corpus needed for the LDA model and for
        coherence measures.

        Parameters
        ----------
        X : pd.Series, np.array or similar one dimensional structure
            Contains the synopses of all movies to examine.

        Returns
        -------
        documents : list
            The documents pre-processed to work with LdaModel.
        dictionary : gensim.corpora.dictionary.Dictionary
            Dictionary of all words and id mappings.
        corpus : list
            List of the documents as a bag of words.
        id2word : mapping
            A mapping from word id to the actual word.

        """
        # Convert the input docs to LDA friendly documents
        documents = self._create_lda_documents(X)
        
        # Create a dictionary representation of the documents            
        dictionary = Dictionary(documents)
        
        # Transform documents to a bag of words reprentation (vectorized form)
        corpus = [dictionary.doc2bow(doc) for doc in documents]
        
        # Make a index to word dictionary.
        temp = dictionary[0]  # This is only to "load" the dictionary.
        id2word = dictionary.id2token
        
        # Update values in the object
        self.documents = documents
        self.dictionary = dictionary
        self.corpus = corpus
        self.id2word = id2word
        
        return documents, dictionary, corpus, id2word
    
    
    def fit(self, X):
        """
        Fit the LdaModel as specifed by the input parameters. Also saves a logfile.
        # The code for the LDA model was inspired by the gensim documentation:
        # https://radimrehurek.com/gensim/auto_examples/tutorials/run_lda.html#pre-process-and-vectorize-the-documents
        
        Parameters
        ----------
        X : pd.Series, np.array or similar one dimensional structure
            Contains the synopses of all the movies in the training data.

        Returns
        -------
        None.

        """
        try: 
            # Attempt to load the model if one already exists
            model = LdaModel.load(f"../Models/gensim/model{self.nr_topics}_{self.nr_passes}pass")
            
        except FileNotFoundError:
            # Create the dictionary, corpus and id2word
            self.create_dictionary(X)
            
            # Create logging file
            logging.basicConfig(filename='gensim.log', format='%(asctime)s:%(levelname)s:%(message)s', level=logging.INFO)

            # Fit the model
            model = LdaModel(
                corpus=self.corpus,
                num_topics=self.nr_topics,
                passes=self.nr_passes,
                alpha="auto", 
                eta="auto",
                id2word=self.id2word,
                random_state=0
                )
            
            # Save the resulting model
            model.save(f"../Models/gensim/model{self.nr_topics}_{self.nr_passes}pass")
            
            # Shutdown the logging file
            logging.shutdown()
        
            self.is_fitted = True
        
        # Save the resulting model in the object
        self.model = model
            
         
    def predict(self, X_test):
        """
        Predict the topics for previously unseen documents.

        Parameters
        ----------
        X_test : pd.Series, np.array or similar one dimensional structure
            Contains the synopses of all the movies in the training data.

        Returns
        -------
        predicted_probs : pd.DataFrame
            A data frame consisting of the predicted probabilites of each topic 
            for all documents in test_docs.

        """
        try:
            # Try reading the file
            predicted_probs = pd.read_csv(f"../Output/gensim/lda{self.nr_topics}_df.csv")
        except FileNotFoundError:
            # Preprocess the documents to work with the model
            test_docs_lda = self._create_lda_documents(X_test)

            # Create test corpus
            test_corpus = [self.dictionary.doc2bow(doc) for doc in test_docs_lda]
            
            # Get the predicted probabilites of belonging to each topic
            model_preds = [doc for doc in self.model.get_document_topics(test_corpus)]
            
            # Get the predicted probabilites of belonging to each topic
            predicted_probs = self._convert_lda_to_df(model_preds, self.nr_topics)
            
            # Save to csv if it does not exist
            predicted_probs.to_csv(f"../Output/gensim/lda{self.nr_topics}_df.csv", index=False)
        
        return predicted_probs
    
    
    def get_topics_per_class(self, y): 
        """
        Count the number of topics per class occurrence. The topics are here 
        generated by argmax, to contrast with BERTopic in which it is chosen
        from HDBSCAN.

        Parameters
        ----------
        y : pd.Series, np.array or similar one dimensional structure
            The genre labels for the plots.

        Returns
        -------
        model_labels : pd.DataFrame
            A data frame consisting of counts of class-topic combinations.

        """
        try: 
            # Try reading the files
            model_probs = pd.read_csv(f"../Output/gensim/model{self.nr_topics}_df.csv")
            model_labels = pd.read_csv(f"../Output/gensim/model{self.nr_topics}_labels.csv")
        except FileNotFoundError:
            # Convert the estimated probabilites for each topics (which may not)
            # include all topics to a data frame with all topics included
            model_probs = self._convert_lda_to_df([doc for doc in self.model.get_document_topics(self.corpus)], 
                                                  nr_topics=self.nr_topics)
            # If file doesn't exist: create it
            model_probs.to_csv(f"../Output/gensim/model{self.nr_topics}_df.csv", index=False)
            
            # Classify the topics by the maximum probability and calculate the
            # size of each combination of Class (truth) and Topic (predicted argmax)
            model_labels = pd.DataFrame([y, np.argmax(model_probs.values, axis=1)]).T.\
                rename(columns={0: "Class", "Unnamed 0": "Topic"}).groupby(["Class", "Topic"]).\
                    size().reset_index().rename(columns={0: "Frequency"})
                    
            # If file doesn't exist: create it
            model_labels.to_csv(f"../Output/gensim/model{self.nr_topics}_labels.csv", index=False)

        return model_labels
    
    
    def coherence_score(self, X):
        """
        Calculate coherence metric for LDA models using NPMI.
        
        Parameters
        ----------
        X : pd.Series, np.array or similar one dimensional structure
            Contains the documents in the training data.

        Returns
        -------
        c_npmi : float
            The coherence score for the generated topics.

        """
        if not self.is_fitted:
            self.create_dictionary(X)
            
        # Calculate coherence score
        c_npmi = CoherenceModel(model=lda_base20.model, 
                                corpus=self.corpus, dictionary=self.dictionary,
                                coherence='c_nmpi').get_coherence()
        return c_npmi
    
    
class BERT_model:
    def __init__(self, min_topic_size=10):        
        """
        Create a new object of the custom BERT_model class used for this report.

        Parameters
        ----------
        min_topic_size : int, the default is 10.
            The minimum size of the topics.

        Returns
        -------
        None.

        """
        # Path to the the BERT model 
        bert_path = 'BERT/all-MiniLM-L6-v2'
        
        # Specify the embedding model
        self.sentence_model = SentenceTransformer(bert_path)
        
        # Specify UMAP model with specfied random state (otherwise default)
        self.umap_model = UMAP(n_neighbors=15, n_components=5, 
                               min_dist=0.0, metric='cosine', random_state=42)
        
        # Save min_topic_size
        self.min_topic_size = min_topic_size
        
        # Topic model with greater topic size and auto topic reduction
        self.topic_model = BERTopic(embedding_model=self.sentence_model, 
                                    calculate_probabilities=True,
                                    n_gram_range=(1,2),
                                    min_topic_size=self.min_topic_size, 
                                    nr_topics="auto",
                                    umap_model=self.umap_model)
        # Placholders
        self.probs = None
        self.topics = None
        
        
    def fit(self, X, y=None):
        """
        Fit the transform on the given data, with or without a class label.
        Will attempt to read the files if they exist.

        Parameters
        ----------
        X : pd.Series, np.array or similar one dimensional structure
            Contains the synopses of all the movies in the training data.
        y : pd.Series, np.array or similar one dimensional structure
            The genres of the movie synopses indexed by integers. 
            The default is None.

        Returns
        -------
        None. Instead the topic model is updated and probabilities and topics
        saved within the class.

        """
        # Specify the model name
        self.model_name = "sup"
        if y is None:
            self.model_name= "unsup"
        
        try:
            # Attempt to read the files
            if y is None:
                # Unsupervised model
                topic_model = BERTopic.load(f"../Models/BERTopic/unsup_bigram_model_auto{self.min_topic_size}")
                topic_df = pd.read_csv(f"../Output/BERTopic/unsup_bigram_topics_auto{self.min_topic_size}.csv")

            else: 
                # Supervised model
                topic_model = BERTopic.load(f"../Models/BERTopic/sup_bigram_model_auto{self.min_topic_size}")
                topic_df = pd.read_csv(f"../Output/BERTopic/sup_bigram_topics_auto{self.min_topic_size}.csv")

            # Split to corresponding entries
            topics = np.array(topic_df["topic"])
            probs = np.array(topic_df.drop("topic", axis=1))
            
            # Update the topic model
            self.topic_model = topic_model
            
        except FileNotFoundError:   
            # If the file does not exist; create it
            if y is None:
                # Unsupervised model
                topics, probs = self.topic_model.fit_transform(X)
                self.topic_model.save(f"../Models/BERTopic/unsup_bigram_model_auto{self.min_topic_size}")
                pd.DataFrame(probs).assign(topic=topics).\
                    to_csv(f"../Output/BERTopic/unsup_bigram_topics_auto{self.min_topic_size}.csv", index=False) 
            else:
                # Supervised
                topics, probs = self.topic_model.fit_transform(X, y=y)
                self.topic_model.save(f"../Models/BERTopic/sup_bigram_model_auto{self.min_topic_size}")
                pd.DataFrame(probs).assign(topic=topics).\
                    to_csv(f"../Output/BERTopic/sup_bigram_topics_auto{self.min_topic_size}.csv", index=False) 
        
        # Save topics and probabilites
        self.topics = topics
        self.probs = probs
        
    
    def get_keywords(self, n=5, n_topics=10):     
        """
        Print the top n keywords in the top n_topics topics.

        Parameters
        ----------
        n : int, the default is 5.
            The number of keywords to print.
        n_topics : int, the default is 10.
            The number of topics to consider.
        Returns
        -------
        None. Results are instead printed.

        """
        for i in range(-1, n_topics):
            pprint(f"Topic number {i}:")
            pprint(dict(self.topic_model.get_topic(i)[:n]))

            
    def get_topic_frequencies(self, n=11):
        """
        Get the topics and their frequencies.

        Parameters
        ----------
        n : int, the default is 11.
            How many topics to return. 

        Returns
        -------
            pd.DataFrame
            Contains the topic index and their respective counts.

        """
        return self.topic_model.get_topic_freq()[0:n]

    
    def get_topics_per_class(self, X, y):          
        """
        Retrieve the topics generated per genre.

        Parameters
        ----------
        X : pd.Series, np.array or similar one dimensional structure
            Contains the synopses of all the movies in the training data.
        y : pd.Series, np.array or similar one dimensional structure
            The genres of the movie synopses indexed by integers. 

        Returns
        -------
        topics_class : pd.DataFrame
            A dataframe over all the combinations of topics and genres, along
            the probability of belonging to each topic.

        """
        try: 
            # Attempt to read the file.
            topics_class = pd.read_csv(f"../Output/BERTopic/topic{self.min_topic_size}class_{self.model_name}.csv")
        except FileNotFoundError:
            topics_class = self.topic_model.topics_per_class(X, self.topics, y)
            topics_class.to_csv(f"../Output/BERTopic/topic{self.min_topic_size}class_{self.model_name}.csv")
        
        return topics_class


    def predict(self, X_test): 
        """
        Predict the topics for previously unseen documents.

        Parameters
        ----------
        X_test : pd.Series, np.array or similar one dimensional structure
            Contains the synopses of all the movies in the training data.

        Returns
        -------
        model_preds_df : pd.DataFrame
            All predicted topics for each document in the test data.

        """
        try:
            # Attempt to read the file.
            model_preds_df = pd.read_csv(f"../Output/BERTopic/topic{self.min_topic_size}preds_{self.model_name}.csv")
        except FileNotFoundError:
            # Predict for the new documents.
            model_preds = self.topic_model.transform(X_test)
            # Convert to data frame and save the file
            model_preds_df = pd.DataFrame(model_preds[1]).assign(topics=model_preds[0])
            model_preds_df.to_csv(f"../Output/BERTopic/topic{self.min_topic_size}preds_{self.model_name}.csv")
        
        return model_preds_df
               
    
    def coherence_score(self, X):
        """
        Calculate coherence metric for BERTopic using NPMI.
        Note: Only works for the documens the model was trained on.

        Author: Maarten Grootendorst
        Source: https://github.com/MaartenGr/BERTopic/issues/90
        
        Parameters
        ----------
        X : pd.Series, np.array or similar one dimensional structure
            Contains the documents in the training data.
        sup : bool, the default is false.
            Whether to use a supervised or unsupervised model.

        Returns
        -------
        c_npmi : float
            The coherence score for the generated topics.

        """
        tpc = pd.read_csv(f"../Output/BERTopic/{self.model_name}_bigram_topics_auto{self.min_topic_size}.csv").iloc[:, -1]
    
        # Preprocess Documents
        bert_documents = pd.DataFrame({"Document": X,
                                      "ID": range(len(X)),
                                      "Topic": tpc})
    
        documents_per_topic = bert_documents.groupby(['Topic'], as_index=False).agg({'Document': ' '.join})
        cleaned_docs = self.topic_model._preprocess_text(documents_per_topic.Document.values)
        vectorizer = self.topic_model.vectorizer_model
        analyzer = vectorizer.build_analyzer()
    
        # Extract features for Topic Coherence evaluation
        words = vectorizer.get_feature_names()
        tokens = [analyzer(doc) for doc in cleaned_docs]
        dictionary = Dictionary(tokens)
        corpus = [dictionary.doc2bow(token) for token in tokens]
        topic_words = [[words for words, _ in self.topic_model.get_topic(topic)] 
                        for topic in range(len(set(tpc))-1)]
    
        # Evaluate coherence
        c_npmi = CoherenceModel(topics=topic_words, 
                                texts=tokens, 
                                corpus=corpus,
                                dictionary=dictionary, 
                                coherence='c_npmi', topn=7).get_coherence()
        return c_npmi
        
    
if __name__ == "__main__":
    # Read data
    df = pd.read_csv("../Data/wiki_movie_plots.csv")

    # Clean and group genres    
    df = group_genre(clean_genre(df))
    
    # df.to_csv("../Data/df_clean.csv", index=False)
    
    # Pre-process the synopses
    docs = process_text(df)
    
    # Fix index to make the BERTopic model work
    df.reset_index(inplace=True)
    docs = docs.reset_index(drop=True)
    y = df["GenreFix"].reset_index(drop=True)
    
    # Split data into training and test
    X_train = docs.loc[df["GenreFix"] != "unknown"]
    X_test = docs.loc[df["GenreFix"] == "unknown"]
    y_train, y_test = convert_to_int(y)
    
    # Reset index so that BERTopic works. 
    # Keep original index so that we know which row in df it corresponds to.
    X_train = X_train.reset_index()
    y_train = y_train.reset_index()
    X_test = X_test.reset_index()
    y_test = y_test.reset_index()
    
    ################################ BERTOPIC #################################
    ### Minimum topic size 10 ###
    # Unsupervised
    unsup10 = BERT_model(min_topic_size=10)
    unsup10.fit(X_train["Plot"])
    unsup10.get_keywords()
    unsup10_topic_freqs = unsup10.get_topic_frequencies()
    unsup10_topic_class = unsup10.get_topics_per_class(X_train["Plot"], y_train[0])
    unsup10_preds = unsup10.predict(X_test["Plot"])
    unsup10.coherence_score(X_train["Plot"])
    
    # Supervised
    sup10 = BERT_model(min_topic_size=10)
    sup10.fit(X_train["Plot"], y_train[0])
    sup10.get_keywords()
    sup10_topic_freqs = sup10.get_topic_frequencies()
    sup10_topic_class = sup10.get_topics_per_class(X_train["Plot"], y_train[0])
    sup10_preds = sup10.predict(X_test["Plot"])
    sup10.coherence_score(X_train["Plot"])
    
    ### Minimum topic size 20 ###
    # Unsupervised
    unsup20 = BERT_model(min_topic_size=20)
    unsup20.fit(X_train["Plot"])
    unsup20.get_keywords()
    unsup20_topic_freqs = unsup20.get_topic_frequencies()
    unsup20_topic_class = unsup20.get_topics_per_class(X_train["Plot"], y_train[0])
    unsup20_preds = unsup20.predict(X_test["Plot"])
    unsup20.coherence_score(X_train["Plot"])
    
    # Supervised
    sup20 = BERT_model(min_topic_size=20)
    sup20.fit(X_train["Plot"], y_train[0])
    sup20.get_keywords()
    sup20_topic_freqs = sup20.get_topic_frequencies()
    sup20_topic_class = sup20.get_topics_per_class(X_train["Plot"], y_train[0])
    sup20_preds = sup20.predict(X_test["Plot"])
    sup20.coherence_score()
    sup20.coherence_score(X_train["Plot"])
    
    ### Minimum topic size 30 ###
    # Unsupervised
    unsup30 = BERT_model(min_topic_size=30)
    unsup30.fit(X_train["Plot"])
    unsup30.get_keywords()
    unsup30_topic_freqs = unsup30.get_topic_frequencies()
    unsup30_topic_class = unsup30.get_topics_per_class(X_train["Plot"], y_train[0])
    unsup30_preds = unsup30.predict(X_test["Plot"])
    unsup30.coherence_score(X_train["Plot"])
    
    # Supervised
    sup30 = BERT_model(min_topic_size=30)
    sup30.fit(X_train["Plot"], y_train[0])
    sup30.get_keywords()
    sup30_topic_freqs = sup30.get_topic_frequencies()
    sup30_topic_class = sup30.get_topics_per_class(X_train["Plot"], y_train[0])
    sup30_preds = sup30.predict(X_test["Plot"])
    sup30.coherence_score(X_train["Plot"])
    

    ############################ LDA ##########################################
    # Number of topics: 20.
    lda_base20 = lda_model(nr_topics=20, nr_passes=50)
    lda_base20.fit(X_train["Plot"])
    lda20_labels = lda_base20.get_topics_per_class(y_train[0])
    lda20_probs = lda_base20.predict(X_test["Plot"])
    pprint(lda_base20.model.print_topics(10, 5))
    lda20_c = lda_base20.coherence_score(X_train["Plot"])
    
    # Number of topics: 40.
    lda_base40 = lda_model(nr_topics=40, nr_passes=50)
    lda_base40.fit(X_train["Plot"])   
    lda40_labels = lda_base40.get_topics_per_class(y_train[0])
    lda40_probs = lda_base40.predict(X_test["Plot"])
    pprint(lda_base40.model.print_topics(10, 5))
    lda40_c = lda_base40.coherence_score(X_train["Plot"])
    
    # Number of topics: 60.
    lda_base60 = lda_model(nr_topics=60, nr_passes=50)
    lda_base60.fit(X_train["Plot"])
    lda60_labels = lda_base60.get_topics_per_class(y_train[0])
    lda60_probs = lda_base60.predict(X_test["Plot"])
    pprint(lda_base60.model.print_topics(10, 5))
    lda60_c = lda_base60.coherence_score(X_train["Plot"])

