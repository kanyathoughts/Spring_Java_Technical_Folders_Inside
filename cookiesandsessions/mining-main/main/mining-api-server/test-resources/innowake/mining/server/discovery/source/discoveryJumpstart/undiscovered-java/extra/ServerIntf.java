import java.rmi.*;
import java.util.Vector;

public interface ServerIntf extends Remote
{
	public boolean validUser(String user, String pass) throws RemoteException;
	public Vector retNotDoneSubList(String sid) throws RemoteException;
	public Vector retTakenSubList(String sid) throws RemoteException;
	public void regSub(Vector v, String sid) throws RemoteException;
}