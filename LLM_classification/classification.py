from openai import OpenAI
from striprtf.striprtf import rtf_to_text
import os
import csv
import sys
from dotenv import load_dotenv  

csv.field_size_limit(2**31 - 1)

load_dotenv()

api_key = os.getenv("OPENAI_API_KEY")

decision_tree_text = ""

#the csv files that we want to classify
target_file_con = "Pre-processed_ENGO_con.csv"
target_file_non_con = "Pre-processed_ENGO_non_con.csv"

prompt="The following text is a written decision issued by a federal judge or court in a civil legal case in the United States. " \
"The legal dispute at issue has to do with an environmental conflict of some sort. For example, the decision could be focused on resolving disputes over the government's role protecting endangered species;" \
"the enforcement of the Clean Air Act or Clean Water Act; or resolving who is finically liable for cleanup costs relating to remediating toxic pollution. (These are just a few examples - there are many other possible topics discussed in the written decisions.)" \
"We need to classify the written decision by its substantive focus. The categories that the decision could be categorized as are Recreational; Legal and Procedural; Energy and Mineral Resources; Military; Disaster Recovery; Conservation; Waste and Pollution.  The following decision tree offers heuristics for deciding which category a judicial opinion should be assigned to:"

decision_tree_path = "decision_tree.txt"

with open(decision_tree_path, "r", encoding="utf-8") as f:
    decision_tree_text = f.read()

client = OpenAI(api_key=api_key)
few_shot = []

with open(target_file_con, newline="", encoding="utf-8") as f:
    reader = csv.reader(f)
    header = next(reader)
    rows_con = list(reader)

for example_row in rows_con[:7]:
    if "NA" in example_row:
        continue
    txt = str(example_row)
    few_shot.append(
      {"role": "user", 
       "content": txt + prompt + decision_tree_text}
    )

    # all the examples are conservations decision texts.
    few_shot.append(
      {"role": "assistant", "content": "Conservation"}
    )

with open(target_file_non_con, newline="", encoding="utf-8") as f:
    reader_non = csv.reader(f)
    header_non = next(reader_non)
    rows_non_con = list(reader_non)

for i, row in enumerate(rows_con[0:],start=0):
    # print(row)
    if "NA" in row:
        print("NA row")
        continue
    plain = str(row)
    
    messages = [
      # a system instruction up front:
      {"role": "system", "content": "You are a legal-opinion classification assistant.  You must output exactly one of the categories. Here are several example classifications:"}
    ]+few_shot+[ 
        #new examples to classify
      {"role": "user",
       "content": plain + prompt 
                    + decision_tree_text
                    # + ".Refer to the decision tree to classify this text. Output only the category name."}
                    + ".Refer to the example classifications and decision tree to classify this text. Output only the category name."}
                    # + ". Classify this text. Output only the category name."}

    ]
    resp = client.chat.completions.create(
      #o4-mini has longer context token limit.
      model="o4-mini",
      # model="gpt-4o",
      #temperature only for gpu-4o
      # temperature=0.0,
      messages=messages
    )

    print(f"Row {i} →", resp.choices[0].message.content.strip())


