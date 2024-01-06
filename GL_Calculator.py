
from datetime import datetime, timedelta
import re
import pandas as pd
import os
import numpy as np

###########################################################################
# GL272
# GL272
# Get desktop path for the current user
#change1
# desktop_path= os.path.join(os.path.expanduser('~'), 'Desktop')
# Path to the 'GL_BALANCING' folder on the desktop
# gl_balancing_path = os.path.join(desktop_path, 'GL_BALANCING')
gl_balancing_path = os.path.join('/app', 'GL_BALANCING')

# Define the file
file_name = 'rowsGLdat2.csv'

# Full path to the file
full_path = os.path.join(gl_balancing_path, file_name)

# Check if the 'GL_BALANCING' folder exists
if os.path.exists(full_path):
    # List all files in the 'GL_BALANCING' folder 
    gl = pd.read_csv(full_path)
    print("Files in 'GL_BALANCING':")
else:
    print("'GL_BALANCING' folder not found in Desktop.")

#Convert 'AMOUNT' to replace 0 with 0.00
gl['DEBITS'] = gl['DEBITS'].astype(str)
gl['CREDITS'] = gl['CREDITS'].astype(str)

gl['CREDITS'] = gl['CREDITS'].str.replace(',', '').str.replace('-', '')
gl['CREDITS'] = gl['CREDITS'].str.replace(',', '').str.replace('-', '')

gl['DEBITS'] = gl['DEBITS'].astype(str).replace({'0':'0.00'})
gl['CREDITS'] = gl['CREDITS'].astype(str).replace({'0':'0.00'})

gl['DEBITS'] = gl['DEBITS'].astype(float)
gl['CREDITS'] = gl['CREDITS'].astype(float)

gl.head(25)

##########################################################################
# Get desktop path for the current user
#chnage2
# desktop_path= os.path.join(os.path.expanduser('~'), 'Desktop')

# Path to the 'GL_BALANCING' folder on the desktop
# gl_balancing_path = os.path.join(desktop_path, 'GL_BALANCING')
gl_balancing_path = os.path.join('/app', 'GL_BALANCING')


# Define the file
file_name = 'rowsOLDbotdat_clean.csv'

# Full path to the file
full_path = os.path.join(gl_balancing_path, file_name)

# Check if the 'GL_BALANCING' folder exists
if os.path.exists(full_path):
    # List all files in the 'GL_BALANCING' folder 
    old = pd.read_csv(full_path)
    print("Files in 'GL_BALANCING':")
else:
    print("'GL_BALANCING' folder not found in Desktop.")
    
old['ACCOUNT NO'] = old['ACCOUNT NO'].astype(str)
old['ACCOUNT NO'] = old['ACCOUNT NO'].apply(lambda x: str(x).rstrip('.0'))
#old['ACCOUNT NO'] = old['ACCOUNT NO'].astype(int)
#old = old.set_index('AMOUNT')

#merge_old = pd.concat([old, old2], axis=1)

# merge_old.reset_index(drop=True, inplace=True)``
#old = merge_old

# Find the index of the row where 'MEMO CODE' is equal to 'BREAK TOTALS'
if 'BREAK TOTALS' in old['MEMO CODE'].values:
    break_totals_index = old[old['MEMO CODE'] == 'BREAK TOTALS'].index[0]
    # Remove rows starting from the 'BREAK TOTALS' row
    old = old.iloc[:break_totals_index]
elif 'LIVE TIME' in old['MEMO CODE'].values:
    live_time_index = old[old['MEMO CODE'] == 'LIVE TIME'].index[0]
else: 
    pass
    

# Create a new column
old['USER_TC'] = (
    old['USER ID'].str[:0] + old['USER ID'].str[:-3] + ' ' + old['SRC'].astype(str)
)

# Remove commas
old['AMOUNT'] = old['AMOUNT'].astype(str).replace({'0':'0.00'})

old.rename(columns={'NAME':'ACCOUNTNAME', 'ACCOUNT NO':'ACCOUNTNUMBER1'}, inplace=True)
old['ACCOUNTNAME'] = old['ACCOUNTNAME'].astype(str).apply(lambda x: re.sub(r'[0-9.,]*', '', x)[:35])
old['ACCOUNTNAME'] = old['ACCOUNTNAME'].str.replace(',', '').str.replace('-', '').str.replace('*', '').str.replace('.00', '').str.replace(':', '').str.replace('DESC', '').str.replace('FEE', '').str.replace('NOTE', '').str.replace('HAS', '')

# Remove commas and hyphens from "AMOUNT" column
old['AMOUNT'] = old['AMOUNT'].str.replace(',', '').str.replace('-', '')
old['AMOUNT'] = old['AMOUNT'].astype(float)
old.drop(columns=['CONTROL', 'GL OFFSET ACCT'], inplace=True)
old.head(50)

####################################################################################

# Get desktop path for the current user
#change3
# desktop_path= os.path.join(os.path.expanduser('~'), 'Desktop')

# Path to the 'GL_BALANCING' folder on the desktop
# gl_balancing_path = os.path.join(desktop_path, 'GL_BALANCING')
gl_balancing_path = os.path.join('/app', 'GL_BALANCING')

# Define the file
file_name = 'rowsREJdatAmounts.csv'

# Full path to the file
full_path = os.path.join(gl_balancing_path, file_name)

# Check if the 'GL_BALANCING' folder exists
if os.path.exists(full_path):
    # List all files in the 'GL_BALANCING' folder 
    rej = pd.read_csv(full_path)
    print("Files in 'GL_BALANCING':")
else:
    print("'GL_BALANCING' folder not found in Desktop.")
    
rej.rename(columns={'Codes':'TRANS_DESCRIPTION', 'Amount':'AMOUNT'}, inplace=True)

###################################################################################
## Read in the Daily Journal

# Get desktop path for the current user
#chnage4
# desktop_path= os.path.join(os.path.expanduser('~'), 'Desktop')

# Path to the 'GL_BALANCING' folder on the desktop
# gl_balancing_path = os.path.join(desktop_path, 'GL_BALANCING')
gl_balancing_path = os.path.join('/app', 'GL_BALANCING')

# Define the file
file_name = 'rowsDTJdat530.csv'

# Full path to the file
full_path = os.path.join(gl_balancing_path, file_name)

# Check if the 'GL_BALANCING' folder exists
if os.path.exists(full_path):
    # List all files in the 'GL_BALANCING' folder 
    dtj = pd.read_csv(full_path)
    print("Files in 'GL_BALANCING':")
else:
    print("'GL_BALANCING' folder not found in Desktop.")

#dtj = dtj.astype(str)
dtj.columns = dtj.columns.str.upper()

# use a lambda function and str.conatins to filter rows with non-digit character
dtj['SRCE'] = dtj['SRCE'].astype(str)
dtj = dtj[dtj['SRCE'].apply(lambda x: x.isdigit())]

# Create a new column
dtj['USER_TC'] = (
    dtj['SOURCECONTROL'].str[:4] + ' ' + dtj['SRCE'].astype(str)
)

# Remove commas and hyphens from "AMOUNT" column
dtj['AMOUNT'] = dtj['AMOUNT'].str.replace(',', '').str.replace('-', '')
dtj['AMOUNT'] = dtj['AMOUNT'].astype(str).replace({'0':'0.00'})
dtj['AMOUNT'] = dtj['AMOUNT'].astype(float)

# Delete the first character in the string of ACCOUNTNAME 
dtj['ACCOUNTNAME'] = dtj['ACCOUNTNAME'].apply(lambda x: x[1:] if x.startswith('A') else x)
dtj['ACCOUNTNAME'] = dtj['ACCOUNTNAME'].apply(lambda x: re.sub(r'[0-9.,]*', '', x)[:35])
dtj['ACCOUNTNAME'] = dtj['ACCOUNTNAME'].str.replace(',', '').str.replace('-', '').str.replace('*', '').str.replace('.00', '').str.replace(':', '').str.replace('CONSUMER', '').str.replace('COMMERCIAL ', '').str.replace('LOAN', '').str.replace('DESC', '').str.replace('FEE', '').str.replace('NOTE', '').str.replace('HAS', '')

dtj = dtj[dtj['AMOUNT'] > 0]

dtj.drop(columns=['ACCOUNTNUMBER2'], inplace=True)

########################################################################
## Match values from the DTJ with values inside the DTJ

# Create a copy
dtj_no_self_match = dtj.copy()

# Filter the copy to remove rows that match
condition = ~((dtj_no_self_match['AMOUNT'].isin(dtj_no_self_match['AMOUNT'])) | (dtj_no_self_match['ACCOUNTNUMBER1'].isin(dtj_no_self_match['ACCOUNTNUMBER1'])))
dtj_no_self_match = pd.concat([dtj_no_self_match[condition], dtj_no_self_match[condition]])

# Create a copy
dtj_matched_dtj = dtj.copy()

# Create a Match_DTJ column for matches identified
dtj_matched_dtj['MATCH_DTJ_TO_DTJ'] = dtj_matched_dtj.duplicated(subset=['AMOUNT', 'ACCOUNTNUMBER1'], keep=False).astype(int)

# Convert column to STR
dtj_matched_dtj["TRAN"] = dtj_matched_dtj["TRAN"].apply(lambda x: str(x))

debits_classifiers = ['310','670','671','675','676','673','678','698','710','711','712','713','714','715','720','725','735','750','751','755']
credit_classifiers = ['330','610','612','613','614','615','617','616','619','620','650','651','653','654','660','663','664','665','668','677','680','97','727','737','770']
totals_classifiers = ['529','662','619','662','713','790','794','795','876']

#UPDATE THE NEW COLUMNS
dtj_matched_dtj['DEBIT_TRAN'] = dtj_matched_dtj['TRAN'].isin(debits_classifiers).astype(int)
dtj_matched_dtj['CREDIT_TRAN'] = dtj_matched_dtj['TRAN'].isin(credit_classifiers).astype(int)
dtj_matched_dtj['TOTALS_TRAN'] = dtj_matched_dtj['TRAN'].isin(totals_classifiers).astype(int)

dtj_matched_dtj['ACCOUNTNUMBER1'] = dtj_matched_dtj['ACCOUNTNUMBER1'].astype(str)

####################################################################################

## GL Copy Matched to dtj
##### These are dtj values where the dtj has already been matched against itself

# Make a copy of the gl df
gl_matched_dtj = gl.copy()

# Create a list of stored values in th dtj that do not match the gl
#matched_dtj = pd.DataFrame(columns=dtj_matched_dtj.columns)

# Function to clean and standardize account numbers
def standardize_account_number(s):
    # Limit the string to the first 12 characters
    return ''.join(filter(str.isdigit, s.split('-')[0].strip())).upper()

gl_matched_dtj['CREDITS'] = gl_matched_dtj['CREDITS'].astype(float)
gl_matched_dtj['DEBITS'] = gl_matched_dtj['DEBITS'].astype(float)
dtj_matched_dtj['AMOUNT'] = dtj_matched_dtj['AMOUNT'].astype(float)

#gl_matched_dtj['CREDITS'] = gl_matched_dtj['CREDITS'].astype(str).replace({'0':'0.00'})
#gl_matched_dtj['DEBITS'] = gl_matched_dtj['DEBITS'].astype(str).replace({'0':'0.00'})
gl_matched_dtj['TRANS DESCRIPTION'] = gl_matched_dtj['TRANS DESCRIPTION'].astype(str)
dtj_matched_dtj['ACCOUNTNUMBER1'] = dtj_matched_dtj['ACCOUNTNUMBER1'].astype(str)

# Initialize MATCH_DTJ columns with 0
gl_matched_dtj['MATCH_DTJ_TO_GL'] = 0
dtj_matched_dtj['MATCH_GL_TO_DTJ'] = 0

gl_matched_dtj['TR_ACCOUNTNUMBER'] = gl_matched_dtj['TRANS DESCRIPTION'].apply(standardize_account_number)
gl_matched_dtj['TR_ACCOUNTNUMBER'] = gl_matched_dtj['TRANS DESCRIPTION'].apply(standardize_account_number)

# Regex search pattern
pattern = r'([^\d]*(\d{8,10})-?'

# Match 'AMOUNT' value from the dtj with 'DEBITS', 'CREDITS' 
# Match 'AMOUNT' value from the dtj with 'DEBITS', 'CREDITS' 
for dtj_index, dtj_row in dtj_matched_dtj.iterrows():
    dtj_amount = dtj_row['AMOUNT']
    dtj_account_number = standardize_account_number(dtj_row['ACCOUNTNUMBER1'])
    
    # Try to find a match in 'DEBITS' or 'CREDITS'  for 'AMOUNT'
    for gl_index, gl_row in gl_matched_dtj.iterrows():
        if dtj_amount == gl_row['DEBITS'] and dtj_account_number == gl_row['TR_ACCOUNTNUMBER']:
            dtj_matched_dtj.at[dtj_index, 'MATCH_GL_TO_DTJ'] = 1
            gl_matched_dtj.at[gl_index, 'MATCH_DTJ_TO_GL'] = 1
            # Fill individual columns from dtj into gl_copy
            if 'USER_TC' in dtj_matched_dtj.columns:
                gl_matched_dtj.at[gl_index, 'USER_TC'] = dtj_row['USER_TC']
            if 'ACCOUNTNUMBER1' in dtj_matched_dtj.columns: 
                gl_matched_dtj.at[gl_index, 'ACCOUNTNUMBER1'] = dtj_row['ACCOUNTNUMBER1']
            if 'ACCOUNTNAME' in dtj_matched_dtj.columns: 
                gl_matched_dtj.at[gl_index, 'ACCOUNTNAME'] = dtj_row['ACCOUNTNAME']
            break
            # Print statement for debugging
            print(f"Checking the dtj account {dtj_account_number}, dtj amount {dtj_amount} against the GL account: {gl_row['TR_ACCOUNTNUMBER']}, and amount {gl_amount}")
    
    # If no match found in 'DEBITS' check credits    
        if dtj_matched_dtj.at[dtj_index, 'MATCH_GL_TO_DTJ'] == 0:
            for gl_index, gl_row in gl_matched_dtj.iterrows():
                if dtj_amount == gl_row['CREDITS'] and dtj_account_number == gl_row['TR_ACCOUNTNUMBER']:
                    gl_matched_dtj.at[gl_index, 'MATCH_DTJ_TO_GL'] = 1
                    dtj_matched_dtj.at[dtj_index, 'MATCH_GL_TO_DTJ'] = 1
                        
                    # Fill individual columns from dtj into gl_copy
                    if 'USER_TC' in dtj_matched_dtj.columns:
                        gl_matched_dtj.at[gl_index, 'USER_TC'] = dtj_row['USER_TC']
                    if 'ACCOUNTNUMBER1' in dtj_matched_dtj.columns: 
                        gl_matched_dtj.at[gl_index, 'ACCOUNTNUMBER1'] = dtj_row['ACCOUNTNUMBER1']
                    if 'ACCOUNTNAME' in dtj_matched_dtj.columns: 
                        gl_matched_dtj.at[gl_index, 'ACCOUNTNAME'] = dtj_row['ACCOUNTNAME']
                    break 
# set the display format on the ACCOUNTNUMBER1 column
#gl_matched_dtj['ACCOUNTNUMBER1'] = gl_matched_dtj['ACCOUNTNUMBER1'].apply('{:.0f}'.format)   

###################################################################################
## GL Copy Matched to Online Dollar

# Make a copy of the gl df
gl_match_dtj_and_old = gl_matched_dtj.copy()

# Initialize binary values
gl_match_dtj_and_old['MATCH_GL_TO_OLD'] = 0
old['MATCH_GL_TO_OLD'] = 0

# Error handle value types
gl_match_dtj_and_old['DEBITS'] = gl_match_dtj_and_old['DEBITS'].astype(str)
gl_match_dtj_and_old['CREDITS'] = gl_match_dtj_and_old['CREDITS'].astype(str)
old['AMOUNT'] = old['AMOUNT'].astype(str)
# gl_match_dtj_and_old['AMOUNT'] = gl_match_dtj_and_old['AMOUNT'].astype(str)
# Match 'AMOUNT' value from the dtj with 'DEBITS', 'CREDITS' 

for index, row in old.iterrows():
    amount =row['AMOUNT']
    
    if amount != 0.00:
        continue
    debit_match = gl_match_dtj_and_old[gl_match_dtj_and_old['DEBITS'] == amount]
    credit_match = gl_match_dtj_and_old[gl_match_dtj_and_old['CREDITS'] == amount]
        
    if not debit_match.empty or not credit_match.empty:
        
        gl_index = debit_match.index[0] if not debit_match.empty else credit_match.index[0]
        gl_match_dtj_and_old.at[gl_index, 'MATCH_GL_TO_OLD'] = 1 
        old.at[gl_index, 'MATCH_GL_TO_OLD'] = 1    
        #else:
        #  gl_match_dtj_and_old = pd.concat([gl_match_dtj_and_old, row.to_frame().T], ignore_index=True)
        
        # Fill individual columns from dtj into gl_copy
        if 'USER_TC' in old.columns:
            gl_match_dtj_and_old.at[gl_index, 'USER_TC'] = row['USER_TC']
        if 'ACCOUNT NO' in old.columns: 
            gl_match_dtj_and_old.at[gl_index, 'ACCOUNT NO'] = row['ACCOUNT NO']
        if 'NAME' in old.columns: 
            gl_match_dtj_and_old.at[gl_index, 'NAME'] = row['NAME']

gl_match_dtj_and_old.rename(columns={'ACCOUNTNUMBER1': 'ACCOUNT_NUMBER', 'ACCOUNTNAME':'ACCOUNT_NAME'}, inplace=True)

gl_match_dtj_and_old.columns = gl_match_dtj_and_old.columns.str.replace(' ', '_')

# Filter out any A/I CONS row values in the "TRANS_DESCRIPTION" column
gl_match_dtj_and_old_filtered = gl_match_dtj_and_old[~gl_match_dtj_and_old['TRANS_DESCRIPTION'].astype(str).apply(lambda x: 'A/I CONS' in x)]

# Filter particular values from the Journal_ID column
values_to_filter = ['LBK', 'NEC','EOD','OLD', np.nan]

gl_match_dtj_and_old_filtered['JOURNAL_ID'] = gl_match_dtj_and_old_filtered['JOURNAL_ID'].astype(str)

# Function to check if any substring is present in the string
def contains_substring(s):
    if isinstance(s, str):
        return not any(substring in s for substring in values_to_filter)
    return True

gl_match_dtj_and_old_filtered = gl_match_dtj_and_old_filtered[~gl_match_dtj_and_old_filtered['TRANS_DESCRIPTION'].astype(str).apply(lambda x: 'LBK' in x)]
gl_match_dtj_and_old_filtered = gl_match_dtj_and_old_filtered[~gl_match_dtj_and_old_filtered['TRANS_DESCRIPTION'].astype(str).apply(lambda x: 'AUTO' in x)]
gl_match_dtj_and_old_filtered = gl_match_dtj_and_old_filtered[~gl_match_dtj_and_old_filtered['TRANS_DESCRIPTION'].astype(str).apply(lambda x: 'ENTRY' in x)]
gl_match_dtj_and_old_filtered = gl_match_dtj_and_old_filtered[~gl_match_dtj_and_old_filtered['TRANS_DESCRIPTION'].astype(str).apply(lambda x: 'PYMTS' in x)]
gl_match_dtj_and_old_filtered = gl_match_dtj_and_old_filtered[~gl_match_dtj_and_old_filtered['TRANS_DESCRIPTION'].astype(str).apply(lambda x: 'OFFSET' in x)]
gl_match_dtj_and_old_filtered = gl_match_dtj_and_old_filtered[~gl_match_dtj_and_old_filtered['TRANS_DESCRIPTION'].astype(str).apply(lambda x: 'ACH' in x)]
gl_match_dtj_and_old_filtered = gl_match_dtj_and_old_filtered[gl_match_dtj_and_old_filtered['ADB_DATE'].apply(lambda x: pd.notna(x))]

###################################################################################################

## Filtering the GL

filtered_gl = gl_match_dtj_and_old_filtered.copy() #[pd.isna(gl_match_dtj_and_old_filtered['USER_TC'])]

# Use a lambda expression to pull out the account numbers
extract_account_number = lambda x: re.search(r'\d{8,}(?=\s|-|$)', str(x)).group(0) if re.search(r'\d{8,}(?=\s|-|$)',str(x)) else None

# Apply the function
filtered_gl['ACCOUNT_NUMBER'] = filtered_gl['TRANS_DESCRIPTION'].apply(extract_account_number)

filtered_gl['ACCOUNT_NUMBER'] = filtered_gl['ACCOUNT_NUMBER'].astype(str)
dtj_matched_dtj['ACCOUNTNUMBER1'] = dtj_matched_dtj['ACCOUNTNUMBER1'].astype(str)


# Custom function to map account names
def mapped_account_names(account_number):
    match = dtj_matched_dtj[dtj_matched_dtj['ACCOUNTNUMBER1'] == account_number]
    if not match.empty:
        return match.iloc[0]['ACCOUNTNAME']
    else:
        return None

filtered_gl['ACCOUNT_NAME'] = filtered_gl['ACCOUNT_NUMBER'].apply(mapped_account_names)

filtered_gl['ON_REC'] = filtered_gl.apply(lambda row: 1 if row['MATCH_GL_TO_OLD'] == 0 and row['MATCH_DTJ_TO_GL'] ==0 else 0, axis=1)

filtered_gl.drop(columns=['ADB_DATE', 'POSTED_DATE', 'ITEM_NUMBER'], inplace=True)
##########################################################################################

## Match the old with the dtj that didn't match with the gl
#### Print only values that didn't match

#dtj_matched_dtj.columns = dtj_matched_dtj.columns.str.strip()
dtj_matched_dtj['ACCOUNTNAME'] = dtj_matched_dtj['ACCOUNTNAME'].str.strip()
old['ACCOUNTNAME'] = old['ACCOUNTNAME'].str.strip()

old_dtj_matched = old.copy()

# Matching must be done with string values only
old_dtj_matched['ACCOUNTNAME'] = old_dtj_matched['ACCOUNTNAME'].astype(str)
old_dtj_matched['AMOUNT'] = old_dtj_matched['AMOUNT'].astype(str)
dtj_matched_dtj['AMOUNT'] = dtj_matched_dtj['AMOUNT'].astype(str)
dtj_matched_dtj['ACCOUNTNAME'] = dtj_matched_dtj['ACCOUNTNAME'].astype(str)

old_dtj_matched['OLD_MATCH_TO_DTJ'] = 0
dtj_matched_dtj['DTJ_MATCH_TO_OLD'] = 0

old['TRAN'] = 0

def preprocess_accountname(s):
    #Remove specific strings
    s = s.replace("commercial", "").replace("loan", "")
    
    # Remove spaces and truncate to 12 characters
    processed_s = s.replace(" ", "")[:12].lower()
    return processed_s

# Function for enhanced string comparison
for old_index, old_row in old_dtj_matched.iterrows():
    if old_row['MATCH_GL_TO_OLD'] == 0:
        # Preprocess 'ACCOUNTNAME
        preprocess_old_accountname = preprocess_accountname(old_row['ACCOUNTNAME'])
        # Check for the exact amount first
        for dtj_index, dtj_row in dtj_matched_dtj.iterrows():
            if dtj_row['MATCH_GL_TO_DTJ'] == 0 and dtj_matched_dtj.at[dtj_index, 'MATCH_DTJ_TO_DTJ'] == 0:
                # Preprocess ACCOUNTNAME
                preprocess_dtj_accountname = preprocess_accountname(dtj_row['ACCOUNTNAME'])
                if old_row['AMOUNT'] == dtj_row['AMOUNT'] and preprocess_old_accountname:
                    # Check if 'ACCOUNTNAME' in old is contained in dtj
                    if preprocess_old_accountname == preprocess_dtj_accountname == preprocess_dtj_accountname and dtj_row['MATCH_GL_TO_DTJ'] == 0 and dtj_row['MATCH_DTJ_TO_DTJ'] == 0:
                        old_dtj_matched.at[old_index, 'OLD_MATCH_TO_DTJ'] =1
                        dtj_matched_dtj.at[dtj_index, 'DTJ_MATCH_TO_OLD'] = 1
                        old_dtj_matched.at[old_index, 'TRAN'] = dtj_row['TRAN']
                        break

##################################################################################
## DEBITS, CREDITS, TOTALS Classifier

dtj_matched_dtj['ON_REC'] = 0

dtj_matched_dtj['ON_REC'] = dtj_matched_dtj.apply(lambda row: 1 if row['MATCH_DTJ_TO_DTJ'] == 0 and row['DTJ_MATCH_TO_OLD'] == 0 and row['MATCH_GL_TO_DTJ'] == 0 else 0, axis=1)
###########################################################

old_dtj_matched.columns = old_dtj_matched.columns.str.strip()
# Convert column to STR
old_dtj_matched["TRAN"] = old_dtj_matched["TRAN"].apply(lambda x: str(x))


debits_classifiers = ['310','670','671','675','676','673','678','698','710','711','712','713','714','715','720','725','735','750','751','755']
credit_classifiers = ['330','610','612','613','614','615','617','616','619','620','650','651','653','654','660','663','664','665','668','677','680','97','727','737','770']
totals_classifiers = ['529','662','619','662','713','790','794','795','876']

#UPDATE THE NEW COLUMNS
old_dtj_matched['DEBITS'] = old_dtj_matched['TRAN'].isin(debits_classifiers).astype(int)
old_dtj_matched['CREDITS'] = old_dtj_matched['TRAN'].isin(credit_classifiers).astype(int)
old_dtj_matched['TOTALS'] = old_dtj_matched['TRAN'].isin(totals_classifiers).astype(int)

old_dtj_matched['ON_REC'] = 0
# Create new column called ON REC
old_dtj_matched['ON_REC'] = old_dtj_matched.apply(lambda row: 1 if row['MATCH_GL_TO_OLD'] == 0 and row['OLD_MATCH_TO_DTJ'] == 0 else 0, axis=1)

######################################################################################

# Filter rows based on matches
old_dtj_no_matches = old_dtj_matched[old_dtj_matched['ON_REC'] == 1]
#####################################################################
# Filter rows based on matches
dtj_no_matches = dtj_matched_dtj[dtj_matched_dtj['ON_REC'] == 1]
dtj_no_matches.rename(columns={'ACCOUNTNAME':'ACCOUNT_NAME','ACCOUNTNUMBER1':'ACCOUNT_NUMBER'}, inplace=True)

######################################################

# Filter rows based on matches
filtered_gl = filtered_gl[filtered_gl['ON_REC'] == 1]

filtered_gl.rename(columns={'ACCOUNTNAME':'ACCOUNT_NAME','ACCOUNTNUMBER1':'ACCOUNT_NUMBER'}, inplace=True)

##################################################################################

old_dtj_matched.columns = old_dtj_matched.columns.str.strip()

# Check if 'TRAN' column 
if 'TRAN' in old_dtj_matched.columns:
    # Convert column to STR
    old_dtj_matched["TRAN"] = old_dtj_matched["TRAN"].apply(lambda x: str(x))
    
    debits_classifiers = ['310','670','671','675','676','673','678','698','710','711','712','713','714','715','720','725','735','750','751','755']
    credit_classifiers = ['330','610','612','613','614','615','617','616','619','620','650','651','653','654','660','663','664','665','668','677','680','97','727','737','770']
    totals_classifiers = ['529','662','619','662','713','790','794','795','876']

    #UPDATE THE NEW COLUMNS
    old_dtj_matched['DEBITS'] = old_dtj_matched['TRAN'].isin(debits_classifiers).astype(int)
    old_dtj_matched['CREDITS'] = old_dtj_matched['TRAN'].isin(credit_classifiers).astype(int)
    old_dtj_matched['TOTALS'] = old_dtj_matched['TRAN'].isin(totals_classifiers).astype(int)

    old_dtj_matched['ON_REC'] = 0
    # Create new column called ON REC
    old_dtj_matched['ON_REC'] = old_dtj_matched.apply(lambda row: 1 if row['MATCH_GL_TO_OLD'] == 0 and row['OLD_MATCH_TO_DTJ'] == 0 else 0, axis=1)
else: 
    pass

old_dtj_matched['ON_REC'] = old_dtj_matched.apply(lambda row: 1 if row['MATCH_GL_TO_OLD'] == 0 and row['OLD_MATCH_TO_DTJ'] == 0 else 0, axis=1)

##########################################################################

combined_final_df['DEBITS'] = combined_final_df['DEBITS'].astype(float)
combined_final_df['CREDITS'] = combined_final_df['CREDITS'].astype(float)
combined_final_df['AMOUNT'] = combined_final_df['AMOUNT'].astype(float)

# Update AMOUNT based on DEBITS and CREDITS
def determine_amount(row):
    if pd.notna(row['DEBITS']) and row['DEBITS'] > 0:
        return row['DEBITS']
    elif pd.notna(row['CREDITS']):
        return row['CREDITS']
    else: 
        return row['AMOUNT']
    
combined_final_df['AMOUNT'] = combined_final_df.apply(determine_amount, axis=1)

# Assign 1 to DEBIT_TRAN where CREDITS > 0
condition_debit = (combined_final_df['DEBITS'] > 0)
combined_final_df.loc[condition_debit, 'DEBIT_TRAN'] = 1

# Assign 1 to CREDIT_TRAN where CREDITS > 0
condition_debit = (combined_final_df['CREDITS'] > 0) 
combined_final_df.loc[condition_debit, 'CREDIT_TRAN'] = 1

# Make AMOUNT negative where DEBIT_TRAN equals 1
combined_final_df['AMOUNT'] = combined_final_df.apply(lambda row: -row['AMOUNT'] if row['DEBIT_TRAN'] == 1 and pd.notna(row['AMOUNT']) else row['AMOUNT'], axis=1)

# Add a 'TOTALS' column
combined_final_df['TOTAL'] = np.nan

# Create a new totals columns
total_amount = combined_final_df['AMOUNT'].sum().astype(float)

# Insert a new column right next to the amount column
new_row_df = pd.DataFrame({'TOTAL': [total_amount]}, index=[0])

# Set NaN values for all other columns in the new row
for col in combined_final_df.columns:
    if col != 'TOTAL':
        new_row_df[col] = np.nan
        
combined_final_df = pd.concat([combined_final_df, new_row_df], ignore_index=True)

columns_to_keep = ['JOURNAL_ID','MEMO CODE','USER_TC','TRAN','ACCOUNT_NUMBER', 'ACCOUNT_NAME', 'TRANS_DESCRIPTION','AMOUNT', 'TOTAL', 'DEBITS', 'CREDITS','DEBIT_TRAN', 'CREDIT_TRAN','FROM_GL','FROM_DTJ','FROM_OLD','FROM_REJ']
combined_final_df = combined_final_df[columns_to_keep]
combined_final_df['DEBIT_TRAN'] = combined_final_df['DEBIT_TRAN'].fillna(0).astype(float).astype(int)
combined_final_df['CREDIT_TRAN'] = combined_final_df['CREDIT_TRAN'].fillna(0).astype(float).astype(int)
combined_final_df['FROM_GL'] = combined_final_df['FROM_GL'].fillna(0).astype(float).astype(int)
combined_final_df['FROM_DTJ'] = combined_final_df['FROM_DTJ'].fillna(0).astype(float).astype(int)
combined_final_df['FROM_OLD'] = combined_final_df['FROM_OLD'].fillna(0).astype(float).astype(int)
combined_final_df['FROM_REJ'] = combined_final_df['FROM_REJ'].fillna(0).astype(float).astype(int)
combined_final_df['TOTAL'] = combined_final_df['TOTAL'].astype(float)

combined_final_df.tail()

##########################################################################

## Print all files

# Get desktop path for the current user
#change5
# desktop_path= os.path.join(os.path.expanduser('~'), 'Desktop')

# Path to the 'GL_BALANCING' folder on the desktop
# gl_balancing_path = os.path.join(desktop_path, 'GL_BALANCING')
gl_balancing_path = os.path.join('/app', 'GL_BALANCING')

# Path to 'REC' folder on the desktop
rec_folder_path = os.path.join(gl_balancing_path, 'REC')

# Define the file
file_name = 'ALL_ITEMS_ON_REC.xlsx'

# Full path to the file
full_path = os.path.join(rec_folder_path, file_name)

# Use ExcelWriter to write each df to a sheet
with pd.ExcelWriter(full_path) as writer:
    combined_final_df.to_excel(writer, sheet_name='RECS', index=False)
    dtj_matched_dtj.to_excel(writer, sheet_name='DAILYJOURNAL', index=False)
    gl_match_dtj_and_old_filtered.to_excel(writer, sheet_name='GL', index=False)
    old_dtj_matched.to_excel(writer, sheet_name='ONLINEDOLLAR', index=False)
print(f"File saved to: {full_path}")
______________________________________