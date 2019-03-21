# loading libraries
from flask import Flask, render_template, url_for, request
import pandas as pd
import numpy as np
import re
import matplotlib.pyplot as plt
import string
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn import naive_bayes
from sklearn import svm
#from sklearn.pipeline import Pipeline
from sklearn.metrics import precision_recall_curve, auc, roc_auc_score, accuracy_score
from sklearn.model_selection import train_test_split, ShuffleSplit
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.linear_model import LogisticRegression
from skmultilearn.problem_transform import BinaryRelevance, ClassifierChain, LabelPowerset
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
#import gensim
#from gensim.corpora import Dictionary
#from gensim.utils import simple_preprocess
#from gensim.parsing.preprocessing import STOPWORDS
from nltk.stem import WordNetLemmatizer, SnowballStemmer
#from nltk.stem.porter import *
from nltk.corpus import stopwords 
#from sklearn.decomposition import NMF, LatentDirichletAllocation
import pickle
from sklearn.externals import joblib

app = Flask(__name__)
@app.route("/")
def home():
        return render_template("home.html")
@app.route("/predict", methods = ["POST"])

def predict():
            # loading data
    train = pd.read_csv("train.csv")
    test = pd.read_csv("test.csv")

    # cleaning comments
    def clean_text(text):
        text = text.lower()
        pat = re.compile(r"[^A-Za-z\s']")
        text = pat.sub(" ", text)
        text = text.rstrip()
        newLines = re.compile(r"[\n\r\t]")
        text = newLines.sub(" ", text)
        extraspace = re.compile(r'\s{2,}')
        text = extraspace.sub(" ", text)
        return text


    eng_stopwords = set(stopwords.words('english'))
    def preprocess_text(text): 
        text = " ".join([word for word in text.split() if len(word) >2])
        text = " ".join([word for word in text.split() if word not in eng_stopwords])
        text = " ".join([word for word in text.split() if word not in ["i'm", "can't"]])
        text = " ".join([WordNetLemmatizer().lemmatize(word) for word in text.split()])
        return text

    train["cleaned_comments"] = train["comment_text"].map(clean_text)
    train["final_cleaned_comments"] = train["cleaned_comments"].map(preprocess_text)

    test["cleaned_comments"] = test["comment_text"].map(clean_text)
    test["final_cleaned_comments"] = test["cleaned_comments"].map(preprocess_text)

    tfidfVect = TfidfVectorizer(min_df = 100, strip_accents = "unicode", stop_words = "english", smooth_idf = True)
    tfidfVect = tfidfVect.fit_transform(train["final_cleaned_comments"])

    train_target = train[["toxic", "severe_toxic","obscene", "threat", "insult", "identity_hate"]]
    X_train_w, X_test_w, y_train_w, y_test_w = train_test_split(train["final_cleaned_comments"], train_target, test_size = 0.3, random_state = 0)
    tfidfVectClean = TfidfVectorizer(min_df = 100, strip_accents = "unicode", stop_words = "english", smooth_idf = True).fit(X_train_w)
    nbModel_w = BinaryRelevance(naive_bayes.MultinomialNB())
    X_train_w_dtm_tfidf = tfidfVectClean.transform(X_train_w)
    nbModel_w.fit(X_train_w_dtm_tfidf, y_train_w)
    predictions = nbModel_w.predict(tfidfVectClean.transform(X_test_w))
    probs = nbModel_w.predict_proba(tfidfVectClean.transform(X_test_w))

    if request.method == "POST":
        message = request.form["message"]
        data = [message]
        vect = tfidfVectClean.transform(data).toarray()
        class_prediction = nbModel_w.predict(vect)
        #toxic_prediction = class_prediction.toarray()[:,0]
        keys = ["toxic", "severe_toxic","obscene", "threat", "insult", "identity_hate"]
        values = class_prediction.toarray()
        t_pred = dict(zip(keys, values[0]))
        tox_labels = [k for k,v in t_pred.items() if v == 1]
        if len(tox_labels) == 0:
            output = "Thank you for keeping your comment respectful to all"
        else:
            output = "Your comment has been flagged as: " + ', '.join(tox_labels) + "."
        
    #return render_template("result.html", prediction = toxic_prediction)
    return render_template("result.html", output = output)

if  __name__ == '__main__':
    app.run(debug = True)
    
            