import pandas as pd

def clean_data(df):
    df['From'] = df['From'].str.strip().str.upper()
    df['Subject'] = df['Subject'].str.strip().str.lower()
    df['Conversation'] = df['Conversation'].str.strip().str.lower()
    return(df)
	
# Importing the requisite libraries for this function
from textblob import TextBlob
import en_core_web_sm
nlp = en_core_web_sm.load()
import pandas as pd

# Defining the function for extracting NLP features from the dataset
def extract_nlp_features(df):
    
    # Initializing empty lists to store the generated data
    Text_Length = []
    Text_Polarity = []
    Text_Subjectiveness = []
    Entity_GeopoliticalEntities = []
    Entity_Organizations = []
    Entity_Persons = []

    # Iterating over each text in the dataset
    for idx, text in df["Text"].iteritems():
      
        # Calculating string length
        Text_Length.append(len(text))
        
        # Using TextBlob's pipeline to process the text data
        textblob_data = TextBlob(text)
        
        # Using Spacy's NLP pipeline to process the text data 
        spacy_data = nlp(text)
        
        # Calculating polarity score (positive vs negative)
        Text_Polarity.append(textblob_data.sentiment.polarity)
        
        # Calculating subjectivity score (objective vs subjective)
        Text_Subjectiveness.append(textblob_data.sentiment.subjectivity)
        
        # Using Spacy to identify named entities
        entities = spacy_data.ents
        
        # Finding Geopolitical Entities named in the text
        Entity_GeopoliticalEntities.append(", ".join([str(i) for i in entities if i.label_ == "GPE"]))
        
        # Finding Organizations named in the text
        Entity_Organizations.append(", ".join([str(i) for i in entities if i.label_ == "ORG"]))
        
        # Finding Persons named in the text
        Entity_Persons.append(", ".join([str(i) for i in entities if i.label_ == "PERSON"]))
        
        # Printing message to console to track progress
        print("Row " + str(idx) + " done...")

    # Assigning results to the dataframe
    df["Text_Length"] = pd.Series(Text_Length)
    df['Text_Polarity'] = pd.Series(Text_Polarity)
    df['Text_Subjectiveness'] = pd.Series(Text_Subjectiveness)
    df["Entity_GeopoliticalEntities"] = pd.Series(Entity_GeopoliticalEntities)
    df["Entity_Organizations"] = pd.Series(Entity_Organizations)
    df["Entity_Persons"] = pd.Series(Entity_Persons)
    
    return(df)
    
# Defining the output schema for returning the dataframe to Tableau Prep
def get_output_schema():
    return pd.DataFrame({
        'Category': prep_string(),
        'Title': prep_string(),
        'Text': prep_string(),
        'Text_Length': prep_int(),
        'Text_Polarity': prep_decimal(),
        'Text_Subjectiveness': prep_decimal(),
        'Entity_GeopoliticalEntities': prep_string(),
        'Entity_Organizations': prep_string(),
        'Entity_Persons': prep_string()
    })