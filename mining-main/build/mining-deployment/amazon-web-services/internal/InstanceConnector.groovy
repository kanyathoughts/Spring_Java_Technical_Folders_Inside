@Grab('com.jcraft:jsch:0.1.54')

import com.jcraft.jsch.ChannelExec
import com.jcraft.jsch.JSch
import com.jcraft.jsch.Session

/**
 * Constants for the instance connector class
 */
class InstConnectConstants {
    static final String LINUX_USER = "ec2-user"
    static final String WINDOWS_USER = "Administrator"

    static enum InstOS {
        LINUX, WINDOWS
    }
}

/**
 * Used to retrieve InstOS.LINUX outside of this class
 * @return InstConnectConstants.InstOS
 */
static InstConnectConstants.InstOS getLinuxInstOS() {
    return InstConnectConstants.InstOS.LINUX
}

/**
 * Used to retrieve InstOS.WINDOWS outside of this class
 * @return InstConnectConstants.InstOS
 */
static InstConnectConstants.InstOS getWindowsInstOS() {
    return InstConnectConstants.InstOS.WINDOWS
}

/**
 * Creates a remote session with a Ec2 instance and returns a session object.
 * @param host Host IP of the instance
 * @param key Private key used to connect to instance
 * @param instance Enum to determine OS of instance
 * @return Session object
 */
static Session createSession(String host, String key, InstConnectConstants.InstOS instance) {
    Properties config = new Properties()
    JSch jsch = new JSch()
    Session session

    try {
        config.put("StrictHostKeyChecking", "no")
        jsch.addIdentity(key)
        if (instance == InstConnectConstants.InstOS.WINDOWS) {
            session = jsch.getSession(InstConnectConstants.WINDOWS_USER, host, 22)
        } else {
            session = jsch.getSession(InstConnectConstants.LINUX_USER, host, 22)
        }
        session.setConfig(config)
        session.connect() /* connect to server via ssh */
        System.out.println("Connected to " + host)
        return session /* return session object for later use */
    } catch (Exception e) {
        e.printStackTrace()
        throw new RemoteCommandException("caused by: $e")
    }
}

/**
 * Executes a command remotely on an Ec2 instance.
 * @param session Session object that contains information about an instance connection
 * @param command Command to be run
 * @param closeAfterExecution Should the session end after execution
 */
void runCommand(Session session, def command, boolean closeAfterExecution) {
    try {
        ChannelExec channel = setUpChannel(session, command)
        InputStream input = channel.getInputStream()
        println "Running command: $command"
        channel.connect() /* connect to channel to run command */
        parseCommandOutput(input, session, channel)
        channel.disconnect() /* close channel */
        if (closeAfterExecution) {
            session.disconnect()
            println "Session closed"
        }
    } catch (RemoteCommandException | NonZeroErrorCodeException e) {
        throw e
    } catch (Exception e) {
        e.printStackTrace()
        throw new RemoteCommandException("caused by: $e")
    }
}

/**
 * Run an SCP command to transfer files to a remote Ec2 instance.
 * @param session Session object that contains information about an instance connection
 * @param source Local source of file
 * @param target Remote target for file
 * @param closeAfterExecution Should the session end after execution
 */
void runScpCommand(Session session, String source, String target, boolean closeAfterExecution) {
    try {
        ChannelExec channel = setUpChannel(session, "scp -t $target") /* set up channel with scp command */
        OutputStream output = channel.getOutputStream()
        InputStream input = channel.getInputStream()
        channel.connect() /* connect to channel */

        File localFile = new File(source)
        String fileName = localFile.getName()
        long fileSize = localFile.length()
        String command = "C0644 $fileSize $fileName\n"
        println "Running command: $command"
        output.write(command.getBytes()) /* send file modes */
        output.flush()

        checkForError(session, input.read()) /* check for error in exit-status */
        /* we do not call parseCommandOutput since we are executing the commands differently */

        println "sending contents of: $fileName"
        FileInputStream fileInputStream = new FileInputStream(localFile)
        byte[] buffer = new byte[102400]
        while (true) {
            int length = fileInputStream.read(buffer, 0, buffer.length)
            if (length <= 0) break
            output.write(buffer, 0, length) /* send contents of files */
        }
        fileInputStream.close()
        buffer[0] = 0
        output.write(buffer, 0, 1)
        output.flush()

        checkForError(session, input.read()) /* check for error in exit-status */

        channel.disconnect() /* close channel */
        if (closeAfterExecution) {
            session.disconnect()
            println "Session closed"
        }
    } catch (RemoteCommandException | NonZeroErrorCodeException e) {
        throw e
    } catch (Exception e) {
        e.printStackTrace()
        throw new RemoteCommandException("caused by: $e")
    }
}

/**
 * Given the session connection, create a channel and set the channel's command.
 * @param session Session object that contains information about an instance connection
 * @param command Command to be run remotely
 * @return The created Channel object
 */
private static ChannelExec setUpChannel(Session session, String command) {
    try {
        ChannelExec channel = session.openChannel("exec")
        channel.setCommand(command)
        channel.setInputStream(null)
        channel.setErrStream(System.err)
        return channel
    } catch (Exception e) {
        e.printStackTrace()
        throw new RemoteCommandException("caused by: $e")
    }
}

/**
 * Parse the command output and send the exit-status to be checked for problems.
 * @param input InputStream that contains output information from the channel
 * @param session Session object that contains information about an instance connection
 * @param channel Channel object that contains the given command
 * @throws NonZeroErrorCodeException
 */
private def parseCommandOutput(InputStream input, Session session, def channel) throws NonZeroErrorCodeException {
    byte[] buffer = new byte[1024]
    int snoozeTimer = 0
    while (true) {
        while (input.available() > 0) {
            int i = input.read(buffer, 0, 1024)
            if (i < 0) break
            System.out.print(new String(buffer, 0, i)) /* print output from command */
        }
        if (channel.isClosed()) {
            println (!input.readLines().isEmpty() ? input.readLines() : "") /* if there is still output, make sure to print it */
            checkForError(session, channel.getExitStatus()) /* check exit-status for errors */
            break
        }
        if (snoozeTimer > 360) {
            throw new NonZeroErrorCodeException("exit-status: command exceeded timeout period")
        } else {
            try {
                Thread.sleep(1000)
                snoozeTimer++
            } catch (Exception e) {
                e.printStackTrace()
            }
        }
    }
}

/**
 * Checks to see if the exit-status passed (0) or failed
 * @param session Session object that contains information about an instance connection
 * @param result the exit-status of a command
 */
private void checkForError(Session session, int result) throws NonZeroErrorCodeException {
    if (result != 0) {
        session.disconnect() /* close session on failure */
        throw new NonZeroErrorCodeException("exit-status: $result detected")
    } else {
        println "exit-status: $result"
    }
}

/**
 * Custom exception to identify when an exist-status was a result other than 0 (success).
 */
class NonZeroErrorCodeException extends Exception {
    NonZeroErrorCodeException(String err) {
        super(err)
    }
}

/**
 * Custom exception to identify when an exception occurs while attempting to run a command remotely.
 */
class RemoteCommandException extends Exception {
    RemoteCommandException(String err) {
        super(err)
    }
}
