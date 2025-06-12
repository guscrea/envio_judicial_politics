from openai import OpenAI
from striprtf.striprtf import rtf_to_text
import os
import csv
import sys
import json

from dotenv import load_dotenv  

csv.field_size_limit(2**31 - 1)

load_dotenv()

api_key = os.getenv("OPENAI_API_KEY")
client = OpenAI(api_key=api_key)

prompt_path = "prompt.txt"
target_file_con = "Pre-processed_ENGO_con.csv"
output_jsonl     = "summaries.json"


#read conservation decision text
with open(target_file_con, newline="", encoding="utf-8") as f:
    reader = csv.reader(f)
    header = next(reader)
    rows_con = list(reader)
#read the prompt
with open(prompt_path, "r", encoding="utf-8") as f:
    prompt = f.read()


out_f = open(output_jsonl, "a", encoding="utf-8")


with open(target_file_con, newline="", encoding="utf-8") as f:
    reader = csv.DictReader(f)   
    for i, row in enumerate(reader,start=0):
        # print(row)
        if "NA" in row["text"]:
            print("NA row")
            continue
        decision_text = row["text"]
        
        messages = [
            {"role": "user", "content": prompt.format(decision_text=decision_text,
                                                decision_id=row["file_name"])}
        ]
        print(messages)
        resp = client.chat.completions.create(
        #o4-mini has longer context token limit.
        model="o4-mini",
        # model="gpt-4o",
        # temperature only for gpu-4o
        # temperature=0.0,
        messages=messages
        )

        model_reply = resp.choices[0].message.content.strip()

        # --- write result as one JSON object per line -----------------------
        result = {
            "file_name": row["file_name"],
            "gpt_output": model_reply
        }
        out_f.write(json.dumps(result, ensure_ascii=False) + "\n")
        # print(f{"row["file_name"]:"}, resp.choices[0].message.content.strip())

