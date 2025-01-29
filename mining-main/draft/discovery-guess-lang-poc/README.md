# GuessLang Language Identification POC

GuessLang is a python library that can be used to detect the programming language of given source code. It supports 54 commonly used languages which it identifies with over 90% accuracy and can be trained to support more languages (not currently implemented). In this PoC, GuessLang is implemented as an API designed to idenitify and classify files it recieves.

# Supported Languages by GuessLang




|  |  |  |  |  |
| --- | --- | --- | --- | --- |
| Assembly | Batchfile | C | C# | C++ |
| Clojure | CMake | COBOL | CoffeeScript | CSS |
| CSV | Dart | DM | Dockerfile | Elixir |
| Erlang | Fortran | Go | Groovy | Haskell |
| HTML | INI | Java | JavaScript | JSON |
| Julia | Kotlin | Lisp | Lua | Makefile |
| Markdown | Matlab | Objective-C | OCaml | Pascal |
| Perl | PHP | PowerShell | Prolog | Python |
| R | Ruby | Rust | Scala | Shell |
| SQL | Swift | TeX | TOML | TypeScript |
| Verilog | Visual Basic | XML | YAML |  |


# Dependencies


The PoC has the following dependencies


Java Version 11+ (11 may cause Exceptions if running in debug mode due to tomcat dependencies but the api should still work, use 17+ if you care about these exceptions)


Python3 Version 3.7+


pip3 or pip


### Python Library Dependencies


* Tensorflow 2.5+ (Python)
* Guesslang (technically) is a dependency however this is just the tool to implement tensor flow. As such, it need not be included in a requirements.txt


### Java Library Dependencies


The following dependencies are included in the pom.xml


* Spring-boot-starter-web
* Spring-boot-devtools
* spring-boot-starter-test
* gson


### System Requirements


* At least 16 GB of system memory is recommended


# Setup Instructions (Local or EC2)


* Open the Zip as a Java Project
* Follow the steps under the Configurations Header (If setting python location through the endpoint skip this step)
* Open your terminal at the root of this project
* cd into the guess lang directory ex. (guesslang-\_\_\_\_\_) "cd guesslang-master"
* type "pip3 install ." or "pip install ." (Depends how path is configured)
  + This should install guesslang
* Go to Poctest123Application.java and start the Spring Boot Server
* Use Postman to test each Endpoint
  + If Endpoints are failing or taking infinite time to return, the path of the python scripts may need to be set using the path of local machine/instance. These can be set by modifying the instance fields corresponding to each script in the LangController.java


# Endpoints


The best way to test the PoC is through Postman or similar service


#### /getguess


* Receives a file and returns a string corresponding to Guesslang's best guess


#### /setconfidinterval


* Recieves a number between 0-100 and sets it as a confidence interval (set the raw body data in postman to your number)


#### /gettopten


* Receives a file and returns a string which displays the top ten Guesses and what percentage guesslang attributes to each guessed language


#### /setpython


* sets the location of python for the processBuilder to the input string (set the raw body data in postman to your python path)


#### /zipsorter


* This file recieves a zip of files that need to be sorted, sorts them according to guesslang and then puts them in directories corresponding to the file type. It then returns the directories zipped.
* Zip Structure should be the following
* myzip.zip
  + File1
  + file2
  + file3


Avoid adding other folders to the zip as it may cause some errors, on mac, use the command:



<YOUR\_DIRECTORY> % zip -r testfiles.zip . -x '.\*' -x '\_\_MACOSX'



# Configurations (Important):


### Python3 Location **(MUST BE CONFIGURED)**


* + The location of python needs to be set for the process builder to execute, this can be done by one of two ways
    - Start the development server and send a post request containing the absolute path of the python location to the endpoint "/setpython"
    - Manually change the pythonLocation String variable in LangController to the location of python on the machine
    + Python3 location needs to be the file ending with python3
    - If you have python added to your path, it should be the same location was what your path for python3 is set too
    - For mac, the default location is set to "/Library/Frameworks/Python.framework/Versions/3.11/bin/python3"
    - On Windows, it defaults to
    - On EC2 it defaults to "/usr/bin/python3"
    - The commands terminal commands which python, or where python can help you locate the python location if it is in your path


### 
Confidence Intervals and New Scripts (Optional)


#### 
Confidence Interval


* The confidence interval for whether a file gets classified or put in the maybe folder can also be configured
* The default value is set to 20% meaning there must be greater than 20% difference of confidence for a file to be classified. Some Examples are given below assuming default confidence interval of 20%
  + Example 1: Guesslang estimates 50% chance the file is a Python file, 25% java and 10% C++ etc. With this result since the difference between the 2 highest probabilities is greater than 20% the file is classified as python
  + Example 2: Guesslang estimates 40% chance the file is C, 33% chance the file is C++ and 5% the file is Ruby etc. With this result, since the difference between the 2 highest probabilities is less than 20% the file is classified as python
* The confidence interval is stored as an int value representing a percent from 0-100. Changes outside of these bounds will not be implemented


#### Python Scripts Locations (Not needed unless adding scripts)


* All python scripts have been placed in the root directory of the project, if Scripts are added for creating endpoints, they must be added to the instance fields and called in the respective ProcessBuilder


# Testing


### How The PoC was tested


Given that GuessLang is an established library with existing tests, this library did not need to be tested. For reference, a confusion matrix of GuessLang is shown below.









Manual testing was performed using Postman to test the endpoints using sample programs for various languages found on the web.



#Here are some sample Calls to the API that can be used
<depoloyedurl> represents the location of where the project is deployed ( for instance localhost:8080 etc.)

curl --location '<deployedurl>/zipsorter' --form 'file
=@<YOUR TEST ZIP>'

curl --location '<deployedurl>/setconfidinterval' --header 'Content-Type: application/json' --data '19'

curl --location '<deployedurl>/getguess' --form 'file
=@<YOUR TEST FILE>'

curl --location '<deployedurl>/gettopten' --form 'file
=@<YOUR TEST FILE>'













# Additional Work Done During PoC Development


### Fully Java Implemented GuessLang TensorFlow Model


* Given that Guesslang is a Python Library that is essentially an implementation of TensorFlow, exploration was undertaken to see if GuessLang could be reproduced in Java due to the fact that Product Codebase is Java based
* Java TensorFlow 1.12.0 was implemented and endpoints were attempted to be created
  + There were many issues with Java Tensorflow as I have detailed below, a primary issue I have faced with it however is that Java Tensorflow is not supported for aarch64 CPU architecture (m1 mac etc.) meaning that developers who use a mac (

         ) are required to develop this on an ec2 instance through SSH
        + This implementation proved difficult as importing the saved\_model.pb file caused many issues with java tensorflow
        + Further issues were created when trying to define tensors and run inferences as the Tensors import was deprecated and
        + Extracting results also proved difficult as TFloat32 was also deprecated making it hard to extract the values
        + Methods that appeared to be returning some datatype in python Tensorflow would return something different in Java Tensorflow, making a one-to-one transcription difficult
        + Ultimately, this proved to be too difficult to implement as Java TensorFlow is poorly maintained and there exists significant gaps in documentation compared to python TensorFlow. This approach was too time-consuming and was eventually abandoned

    


### Training of the GuessLang for more Languages


* Potential exists to use Product reference files to train guessLang to recgonize even more languages including ones that are more useful to product such as jcl
* This would require modification to the guessLang training method in guess.py to increase the language map and provision of source code files for new languages.
* The tool [GuesslangTools](https://github.com/yoeo/guesslangtools) could be used to find additional source files (I am not sure how much files we currently have to be able to train this model on different languages)
  + If sources already exist, part of the tool could still be implemented to turn these into train/test datasets
* Should this PoC decide to be fully developed, this would be a good idea to implement.


# Development
