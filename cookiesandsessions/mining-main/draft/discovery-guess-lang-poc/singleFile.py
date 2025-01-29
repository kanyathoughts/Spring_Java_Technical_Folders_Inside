import sys
from guesslang import Guess

def solve(input):
    guess = Guess()
    lang = guess.language_name(input)
    return lang

if __name__ == "__main__":
    inputCode = ""
    for line in sys.stdin:
        inputCode = inputCode + line
    response = solve(inputCode)
    print(response)
    sys.stdout.flush()



