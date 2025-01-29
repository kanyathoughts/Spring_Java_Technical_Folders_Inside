import sys
from guesslang import Guess
import json

def solve(input):
    guess = Guess()
    lang = guess.probabilities(input)
    return lang

if __name__ == "__main__":
    inputCode = ""
    for line in sys.stdin:
        inputCode = inputCode + line
    response = solve(inputCode)
    topper = response[0:10]
    print(topper)
    sys.stdout.flush()