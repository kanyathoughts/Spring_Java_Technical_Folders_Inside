import groovy.json.JsonOutput
import groovy.json.JsonSlurper
import org.apache.ivy.util.StringUtils

/**
 * Constants for the Utility class.
 */
class UtilConstants {
    static enum InputOption {
        OPTIONAL_FILE, MANDATORY_FILE, OPTIONAL_STRING
    }
}

/**
 * Allows InputOption.OPTIONAL_FILE to be accessible outside this class.
 * @return UtilConstants.InputOption
 */
static UtilConstants.InputOption getOptFile() {
    return UtilConstants.InputOption.OPTIONAL_FILE
}

/**
 * Allows InputOption.MANDATORY_FILE to be accessible outside this class.
 * @return UtilConstants.InputOption
 */
static UtilConstants.InputOption getManFile() {
    return UtilConstants.InputOption.MANDATORY_FILE
}

/**
 * Allows InputOption.OPTIONAL_STRING to be accessible outside this class.
 * @return UtilConstants.InputOption
 */
static UtilConstants.InputOption getOptString() {
    return UtilConstants.InputOption.OPTIONAL_STRING
}

/**
 * Causes the script to sleep for a specified amount of time.
 * @param message Message to be printed about sleep
 * @param seconds Amount of time in seconds
 */
static void snooze(String message, int seconds) {
    try {
        println message
        Thread.sleep(1000 * seconds)
    } catch (Exception ee) {
        ee.printStackTrace()
    }
}

/**
 * Reads the specified file and sets it to the resourceMap variable.
 * @param parsable Object that contains parsable json
 */
static def readJson(def parsable) {
    def jsonSlurper = new JsonSlurper()
    try {
        return jsonSlurper.parse(parsable)
    } catch (Exception e) {
        e.printStackTrace()
    }
}

/**
 * Writes a json map object to the specified file.
 * @param map Map object that contains information about the AWS environment
 * @param fileName File to be created
 */
static void writeJsonFile(def map, String fileName) {
    def jsonString = JsonOutput.toJson(map)
    def json = JsonOutput.prettyPrint(jsonString)
    try {
        File file = new File(fileName)
        file.write(json)
    } catch (Exception e) {
        e.printStackTrace()
    }
}

/**
 * Prints a map in a readable json format.
 * @param map LinkedHashMap to print
 */
static void printJson(LinkedHashMap map) {
    def jsonString = JsonOutput.toJson(map)
    println JsonOutput.prettyPrint(jsonString)
}

/**
 * Deletes the specified file.
 * @param fileName File to be deleted
 */
static void deleteFile(String fileName) {
    try {
        File file = new File(fileName)
        file.delete()
    } catch (Exception e) {
        e.printStackTrace()
    }
}

/**
 * Replaces all special characters in a String with the character '!' which is able to be parsed by batch scripts.
 * @param string String to be parsed
 * @return Parsed string
 */
static String replaceSpecialCharacters(String string) {
    return string.replaceAll("[^^0-9a-zA-Z]+", "!")
}

/**
 * Prints a block of test to show information.
 * @param string Message to be printed
 */
static void printStage(String string) {
    String bracket = "************" + StringUtils.repeat("*", string.length())
    println(bracket)
    println("*-----$string-----*")
    println(bracket)
}

/**
 * Just a fun ASCII title.
 */
static void printAsciiTitle() {
    println("\n" +
            " _____ _                 _  ___  ____       _             \n" +
            "/  __ \\ |               | | |  \\/  (_)     (_)            \n" +
            "| /  \\/ | ___  _   _  __| | | .  . |_ _ __  _ _ __   __ _ \n" +
            "| |   | |/ _ \\| | | |/ _` | | |\\/| | | '_ \\| | '_ \\ / _` |\n" +
            "| \\__/\\ | (_) | |_| | (_| | | |  | | | | | | | | | | (_| |\n" +
            " \\____/_|\\___/ \\__,_|\\__,_| \\_|  |_/_|_| |_|_|_| |_|\\__, |\n" +
            "                                                     __/ |\n" +
            "                                                    |___/ ")
}

/**
 * Requests information from the user during script execution.
 * @param requestMessage Message to be displayed to the user about request
 * @param successMessage Message to be shown upon successful completion of request (for file only)
 * @param status Enum that defines state of the request
 * @param matchCriteria What should we check for to find a successful match (for file only)
 * @param failedMatch Message to be displayed when a match fails (for file only)
 * @return The item supplied by the user
 */
static String provideItem(String requestMessage, String successMessage, UtilConstants.InputOption status, String matchCriteria, String failedMatch) {
    boolean optional = true, objectIsFile = true
    String item

    if (status == UtilConstants.InputOption.MANDATORY_FILE) {
        optional = false
    } else if (status == UtilConstants.InputOption.OPTIONAL_STRING) {
        objectIsFile = false
    }
    /* handle status so we can process request */

    while (item == null) {
        def option = System.console().readLine "$requestMessage ${(optional ? "Enter 'n' to skip this step. " : "")}"
        if (optional && option.equalsIgnoreCase("n")) {
            item = "" /* make string empty if user answers "no" */ 
        } else if (objectIsFile) {
            File file = new File(option.toString())
            if (file.exists() && (matchCriteria == "" ? true : file.getName().contains(matchCriteria))) {
                item = file.getAbsolutePath()
                println "$successMessage"
            } else if (!file.getName().contains(matchCriteria)) {
                println "$failedMatch"
            } else {
                println "Sorry, could not find ${file.getName()} at that location."
            }
        } else {
            item = option.toString()
        }
    }
    return item
}

