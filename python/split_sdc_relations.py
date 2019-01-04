# -*- coding: utf-8 -*-
"""
Created on Sat May 12 10:42:45 2018

@author: T430

Split SDC output for relations (eg, alliances)
into separate rows for each member of the relation 


Positional Arguments:
   filename
   [directory_path]  ##optional


Example Call:
   $ python  <path/to/script>.py  data_file.csv 

Example Call (from outside data directory):
   $ python  <path/to/script>.py  data_file.csv  /home/users/T430/data
   
"""

import os, re, sys
from uuid import uuid4
import pandas as pd
import datetime as dt


## pattern used to select columns to count number of relations 
## in each alliance / joint venture
## here using columns with ultimate parent CUSIP
COUNT_COL = 'ultimate_parent_cusip'
### column names to force string type
#STRING_COLS = ['sic','cusip']


def timestamp():
   """ RETURNS the unix epoch timestamp (example: 1526181030)
   """
   t = dt.datetime.utcnow()
   dt1 = dt.datetime(t.year,t.month,t.day,t.hour,t.minute,t.second)
   dt0 = dt.datetime(1970,1,1,1,1,1)
   tdiff = dt1 - dt0
   return int(tdiff.total_seconds())
   
def processHeader(string, pattern = r'[^a-zA-Z]+', repl='_'):
   """ Convert problematic data frame heading text to lower_snake_case
   """
   header = string.rstrip().replace('\r','').replace('-\n','').lower()
   header = re.sub('\s+', '_', header)
   header = re.sub(pattern, repl, header)
   while re.match(pattern, header[-1]):
      header = header[:-1] ## remove trailing non-alphanumeric
   while re.match(pattern, header[0]):
      header = header[1:] ## remove leading non-alphanumeric 
   return header

def dedupDict(heading_map):
   """ fix duplicate values in a dict mapping by count indexing each
       after the first val,val_2,val_3,...
   """
   if not isinstance(heading_map, dict):
      raise Exception('heading_map must be a dict-like object')
   check = {}
   for key in heading_map.keys():
      val = heading_map[key]
      check[val] = check[val]+1 if val in check else 1
      if check[val] > 1:
         heading_map[key] = '{h}_{i}'.format(h=val,i=check[val])
   return heading_map

def extract(x, index, sep='\n'):
   """ Extract an element of a joined string
   """
   if not isinstance(x, str):
      return x
   items = x.split(sep)
   if index >= len(items):
      return x
   else:
      return items[index]
   
def extractDf(df, index, sep='\n'):
   """ Extract an element of a joined string (by index) for each row,col of data frame
   """
   if not isinstance(df, pd.DataFrame):
      return df
   for col in df.columns:
      df[col] = df[col].apply(lambda x: extract(x,index,sep))
   df.drop_duplicates(inplace=True)
   return df


def main():
   """ Main script
   """
   file_ext_err_msg = 'File extion cannot be converted. Must be csv, xls, xlsx'
   #pyscript = sys.argv[0]  
   args = sys.argv[1:]
   
   if len(args) < 1:
      sys.exit("must provide file to convert")
   
   filename = args[0]
   ext = filename.split('.')[-1]
   dirname = args[1] if len(args) > 1 else os.getcwd()
   filepath = os.path.join(dirname,filename)
   print(' file name: %s' % filename)
   print(' extension: %s' % ext)
   print(' directory: %s' % dirname)
   print(' full path: %s' % filepath)
   
   ## exit if not convertible file type
   if ext not in ['csv','xls','xlsx']:
      sys.exit(file_ext_err_msg)
   
   ## exit if file missing
   if filename not in os.listdir(dirname):
      sys.exit("file not in directory")
   
   ## import data frame
   if ext == 'csv':
      df = pd.read_csv(filepath, low_memory=False)
   elif ext in ['xls','xlsx']:
      df = pd.read_excel(filepath, low_memory=False)
   else:
      sys.exit(file_ext_err_msg)
   #print(df.columns)
   
   ## fix heading strings
   #heading_map = {x:'{i}_{h}'.format(i=i,h=processHeader(x)) for i,x in enumerate(df.columns)}
   heading_map = dedupDict({x:processHeader(x) for x in df.columns})
   ## rename columns by mapping
   df = df.rename(columns=heading_map)
   #print(df.columns)
   
   ## add alliance/jv entity IDs
   df['coop_uuid'] = df[df.columns[0]].apply(lambda x: str(uuid4()))
   df['coop_id'] = pd.Series(range(1,df.shape[0]+1))
   
   ## find number of parties from each alliance / jv relation
   row_counts = df[COUNT_COL].apply(lambda x: len(x.split('\n')) if isinstance(x,str) else 1)
   max_counts = row_counts.max()
   N,M = df.shape
   
   ## MAIN LOOP: extract entries for each party in the alliance / jv
   print(' extracting relations...')
   df_list = []
   for index in range(0, max_counts):
      df_sub_i = df.loc[row_counts > index, :].copy()
      df_list.append(extractDf(df_sub_i, index))
   dfall = pd.concat(df_list).reset_index(drop=True)
   print(' done.')
   
   ## drop duplicates and sort by relation id
   dfall.drop_duplicates(inplace=True)
   dfall.sort_values('coop_id', inplace=True)
   
   ## add individual company-alliance/jv relation IDs
   dfall['uuid'] = dfall[dfall.columns[0]].apply(lambda x: str(uuid4()))
   dfall['id'] = pd.Series(range(1,dfall.shape[0]+1))
   
   ## file extension
   #ext2 = ext  ## keeping same file extension
   ext2 = 'csv' ## convert all to csv
   
   ## remove the extension (only the last part after final "."; keeping all other ".")
   filebase = '.'.join(filename.split('.')[:-1])
   filename2 = '%s-SPLIT%s.%s' %(filebase,timestamp(),ext2)
   filepath2 = os.path.join(dirname,filename2)
   
   # save to new file
   if ext2 == 'csv':
      print(' output at: %s' % filepath2)
      dfall.to_csv(filepath2, index=False)
   elif ext2 in ['xls','xlsx']:
      print(' output at: %s' % filepath2)
      dfall.to_excel(filepath2, index=False)
   else:
      print(file_ext_err_msg)


if __name__ == '__main__':
   main()


