import pandas as pd
from bs4 import BeautifulSoup
from nltk.corpus import stopwords
import re
import string


# Load datasets
dataframes = {
    "cooking": pd.read_csv("../input/cooking.csv"),
    "crypto": pd.read_csv("../input/crypto.csv"),
    "robotics": pd.read_csv("../input/robotics.csv"),
    "biology": pd.read_csv("../input/biology.csv"),
    "travel": pd.read_csv("../input/travel.csv"),
    "diy": pd.read_csv("../input/diy.csv"),
}


url_re = r'(?i)\b((?:https?://|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:\'".,<>?«»“”‘’]))'


def stripTagsAndUrls(x):
	"""
    Function to return a data frame composed of strings without HTML tags and URLs
    
    Args:
        x (Pandas Dataframe): Dataframe with HTML tags and URL contents
    
    Returns:
        Pandas Dataframe without without HTML tags and URLs
    """
    if x:
        # BeautifulSoup on content
        soup = BeautifulSoup(x, "html.parser")
        # Stripping all <code> tags with their content 
        if soup.code:
            soup.code.decompose()
        # Extract text from HTML
        text =  soup.get_text()
        # Returning text stripping out all URLs 
        return re.sub(url_re, "", text)
    else:
        return ""


def removePunctuation(x):
	"""
    Function to return a data frame with no punctuation
    
    Args:
        x (Pandas Dataframe): Dataframe with punctuation
    
    Returns:
        Pandas Dataframe without without punctuation
    """
    x = x.lower()
    # Removing non ASCII chars
    x = re.sub(r'[^\x00-\x7f]',r' ',x)
    # Removing all the punctuation by replacing it with empty spaces actually
    return re.sub("["+string.punctuation+"]", " ", x)


# Remove stopwords from titles and contents
stops = set(stopwords.words("english"))

def removeStopwords(x):
	"""
    Function to return a data frame with no English stopwords, such as 
    "i.e.", "would", "get", "like", "using", "know", "question", "use".
    
    Args:
        x (Pandas Dataframe): Dataframe containing stowords
    
    Returns:
        Pandas Dataframe without without stopwords
    """
    filtered_words = [word for word in x.split() if word not in stops]
    return " ".join(filtered_words)


# Apply the above-defined functions
for df in dataframes.values():
    df["content"] = df["content"].map(stripTagsAndUrls)

for df in dataframes.values():
    df["title"] = df["title"].map(removePunctuation)
    df["content"] = df["content"].map(removePunctuation)

for df in dataframes.values():
    df["title"] = df["title"].map(removeStopwords)
    df["content"] = df["content"].map(removeStopwords)


# Finally, save pre-processed dataframes into a csv format 
for name, df in dataframes.items():
    # Saving to file
    df.to_csv(name + "_preprocessed.csv", index=False)



