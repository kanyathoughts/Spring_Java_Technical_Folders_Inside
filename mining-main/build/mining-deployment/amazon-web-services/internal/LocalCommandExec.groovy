/**
 * Creates an instance of the Utility class and returns it.
 * @return Utility class object
 */
private static Utility getUtil(){
    return new Utility()
}

/**
 * Uses the AWS CLI to request the decrypted password for an Ec2 Windows instance.
 * @param profile The CLI profile with the correct permissions to run command.
 * @param instanceId The ID of specified instance.
 * @param privateKey The private key used to access the instance - decrypts the password.
 * @return The decrypted password.
 */
static String getWindowsPassword(String profile, String instanceId, String privateKey) {
    String password = ""
    int attempts = 0
    while (password == "") {
        Byte[] result = runCommand("aws ec2 get-password-data --profile ${profile} " +
                "--instance-id ${instanceId} --priv-launch-key ${privateKey} --no-verify-ssl")
        /* we must use --no-verify-ssl due to the Deloitte VPN */

        String commandResult = getUtil().readJson(result).PasswordData /* get the PasswordData object from the returned JSON */
        if (commandResult == "") {
            if (attempts < 5) {
                getUtil().snooze("Password is not yet available - waiting one minute to try again...", 60)
                /* we wait if password isn't ready yet */
                attempts++
            } else {
                throw new PasswordTimeoutException("Attempts to get password has exceeded limit.")
                /* throw custom exception after 5 password attempts */
            }
        } else {
            password = commandResult
        }
    }
    return password
}

/**
 * Executes a command on the local machine and returns the output.
 * @param command Command to be run.
 * @return Command's output.
 */
private static Byte[] runCommand(String command) {
    try {
        def sysOut = new ByteArrayOutputStream(), sysErr = new ByteArrayOutputStream()
        println "Executing command (${command}) locally."
        def proc = command.execute()
        proc.consumeProcessOutput(sysOut, sysErr)
        proc.waitForOrKill(5000)
        println "OUT>\n$sysOut\nERR>\n$sysErr"
        return sysOut.toByteArray()
    } catch (Exception e) {
        e.printStackTrace()
        throw new LocalCommandException("An exception ($e) was caught while running the command ($command) locally.")
    }
}

/**
 * Custom exception to identify when an exception is thrown while running a command.
 */
class LocalCommandException extends Exception {
    LocalCommandException(String err) {
        super(err)
    }
}

/**
 * Custom exception to identify when number of password attempts has been exceeded.
 */
class PasswordTimeoutException extends Exception {
    PasswordTimeoutException(String err) {
        super(err)
    }
}
