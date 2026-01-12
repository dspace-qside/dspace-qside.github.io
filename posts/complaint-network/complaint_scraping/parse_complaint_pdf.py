""" # ##########################################################################################
This file parses the complaints data scraped from the Minneapolis Police Officer Complaint History Dashboard
https://tableau.minneapolismn.gov/views/PublicProfileCardFinder/ComplaintCardFinder?%3Aembed=y&%3AisGuestRedirectFromVizportal=y

The data was downloaded using the following steps:
- For each card, click the small arrow in the top right and select "Exclude Values"
    ** this only works for the first two cards **
- In the search bar, type something like "a" and select the + icon within the search bar
- The table should now display all data, since there is no entry "a" and that is the only exclusion
- In the lower right corner of the entire screen, click the download icon (near the Share icon) and select PDF
- Select Include > Specific sheets from this dashboard
- Select all cards from the list, Scaling = "At most 1 page wide", 
                                  Page size = "Unspecified", 
                                  Orientation = 'Portrait'
- Download as a PDF and save in the same directory as this code

This file will parse the Complaints made to Internal Affairs or Office of Police Conduct Review in new data system 
and the Complaints made to the Office of Police Conduct Review in old data system cards. 

The outputs are two csv files, one containing the new complaints and one containing the old complaints. 
########################################################################################## """

from PyPDF2 import PdfReader
import pandas as pd


# function to correct when name is listed on two lines
def correct_two_line_name(df):
    iflag = []
    i = 0
    while i < (len(df)-1):
        if df.loc[i,'is_name'] & df.loc[i+1,'is_name']:
            df.loc[i,'text'] = df.loc[i,'text'] + ' ' + df.loc[i+1,'text'] # combine two-line name into first line
            iflag.append(i+1) # mark second line instance to be dropped
            i = i+2
        else: 
            i = i+1
    df.drop(iflag, inplace=True) # drop second line name
    df.reset_index(drop=True, inplace=True)
    return df


# function to extract and organize the data from the PDF reader function
def extract_and_format(fname, pagenum):
    xlist , ylist, text = [], [], []
    def visitor_before(op, args, cm, tm, ff):
        if ((op != '') & (op != '\n')) :
            x = cm[4]
            y = cm[5]
            text.append(op)
            xlist.append(x)
            ylist.append(y)

    reader = PdfReader(fname)
    number_of_pages = len(reader.pages)
    page = reader.pages[pagenum]
    tt = page.extract_text(visitor_text=visitor_before)
    df = pd.DataFrame({'text': text, 'x': xlist,'y': ylist})
    return df


# ================================================================================================================ 
# This code block formats the complaints from the "Office of Police Conduct Review in old data system", September 2012 through late February 2023
# make sure that this is actually the first page of your PDF, if not, change page number to 1 instead of 0
# ================================================================================================================ 

# Read in the PDF and save x and y locations of text
df = extract_and_format("Public Profile Card Finder.pdf", 0)

# determine the name columns by x location
df['is_name'] = df['x'] < 100

# clean up some known messiness
df = df[ df['text']!=' \n']
df = df[ df['text']!='A.']
df.reset_index(drop=True, inplace=True)

# correct for when name is on two lines
df = correct_two_line_name(df)

# prep the line counts
y_counts = df['y'].value_counts()
df['line_counts'] = df['y'].map(y_counts)
df.reset_index(drop=True, inplace=True)

# looping through the ordered list of entries and organizing into proper columns
# this also back-fills for missing name, report number, status, and action if they are missing within the row
col1, col2, col3, col4, col5 = [], [], [], [], []
i = 0

while i < (len(df)-6):
    if df.loc[i,'line_counts'] == 5:
        col1.append(df.loc[i,'text'])
        col2.append(df.loc[i+1,'text'])
        col3.append(df.loc[i+2,'text'])
        col4.append(df.loc[i+3,'text'])
        col5.append(df.loc[i+4,'text'])
        i+=5
    elif df.loc[i,'line_counts'] == 4:
        col1.append(col1[-1])
        col2.append(df.loc[i,'text'])
        col3.append(df.loc[i+1,'text'])
        col4.append(df.loc[i+2,'text'])
        col5.append(df.loc[i+3,'text'])
        i+=4
    elif df.loc[i,'line_counts'] == 3:
        col1.append(col1[-1])
        col2.append(col2[-1])
        col3.append(df.loc[i,'text'])
        col4.append(df.loc[i+1,'text'])
        col5.append(df.loc[i+2,'text'])
        i+=3
    elif df.loc[i,'line_counts'] == 2:
        col1.append(col1[-1])
        col2.append(col2[-1])
        col3.append(col3[-1])
        col4.append(df.loc[i,'text'])
        col5.append(df.loc[i+1,'text'])
        i+=2
    elif df.loc[i,'line_counts'] == 1:
        col1.append(col1[-1])
        col2.append(col2[-1])
        col3.append(col3[-1])
        col4.append(col4[-1])
        col5.append(df.loc[i,'text'])
        i+=1
    else:
        print('error')

# saving the data frame to csv 
df2 = pd.DataFrame({col1[0]: col1, 
                    col2[0]: col2, 
                    col3[0]: col3, 
                    col4[0]: col4, 
                    col5[0]: col5})
df2 = df2.loc[1:,]
df2.to_csv('old_complaints.csv')

# ================================================================================================================ 
# This code block formats the table from "Internal Affairs or Office of Police Conduct Review in new data system", filed after February 2023
# make sure that this is actually the second page of your PDF, if not, change page number to 0 instead of 1
# ================================================================================================================ 

# Read in the PDF and save x and y locations of text
df = extract_and_format("Public Profile Card Finder.pdf", 1)

# clean up some known messiness including 
df = df[ df['text']!=' \n']
df = df[ df['text']!='A.']
df.reset_index(drop=True, inplace=True)

# determine the name columns by x location
df['is_name'] = df['x'] < 150

# determine case number (starting EVT-) with string match 
df['is_num'] = df['text'].str.contains('EVT-')

# correct for when name is on two lines
df = correct_two_line_name(df)

# # looping through the ordered list of entries and organizing into proper columns
# # this also back-fills for missing name, report number, status, and action if they are missing within the row
col1, col2, col3 = [], [], []
i = 0

while i < (len(df)-3):
    if df.loc[i,'is_name']:
        col1.append(df.loc[i,'text'])
        col2.append(df.loc[i+1,'text'])
        col3.append(df.loc[i+2,'text'])
        i+=3
    elif df.loc[i,'is_num']:
        col1.append(col1[-1])
        col2.append(df.loc[i,'text'])
        col3.append(df.loc[i+1,'text'])
        i+=2
    else:
        i+=1
        print('err')

df3 = pd.DataFrame({col1[0]: col1, 
                    col2[0]: col2, 
                    col3[0]: col3})
df3 = df3.loc[1:,]
df3.to_csv('new_complaints.csv')