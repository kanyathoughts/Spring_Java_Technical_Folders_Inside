import sys
from guesslang import Guess
import json

if len(sys.argv) != 2:
    print("Usage: python script.py <integer>")
    sys.exit(1)

confidenceInterval = int(sys.argv[1])
fullCode =""
#string_arg = sys.argv[2]
for line in sys.stdin:
    fullCode = fullCode + line + "\n"

guess = Guess()
langProbs = guess.probabilities(fullCode)
topPreds = langProbs[0:2]
firstProb = topPreds[0][1] *100
SecondProb = topPreds[1][1] *100
if((firstProb-SecondProb) > confidenceInterval):
    print(topPreds[0][0])
else:
    print("maybe%"+topPreds[0][0])
