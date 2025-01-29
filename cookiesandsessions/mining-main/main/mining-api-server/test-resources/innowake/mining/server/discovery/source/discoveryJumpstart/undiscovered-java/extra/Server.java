import java.util.Vector;
import java.rmi.*;
import java.rmi.server.*;
import java.sql.*;

class ServerImpl extends UnicastRemoteObject implements ServerIntf
{
	public ServerImpl() throws RemoteException
	{
		super();
	}

	public void regSub(Vector v, String sid)
	{
		System.out.println("* Client: void regSub(Vector, "+sid+")");

		try
		{
			Class.forName("sun.jdbc.odbc.JdbcOdbcDriver");
			Connection con = DriverManager.getConnection("jdbc:odbc:db","","");
			Statement stm = con.createStatement();
			stm.executeUpdate("UPDATE sub SET status='Not done' WHERE sid='"+sid+"' AND status='Taken'");
			for(int i=0; i<v.size(); ++i)
				stm.executeUpdate("UPDATE sub SET status='Taken' WHERE sid='"+sid+"' AND sub='"+v.get(i)+"'");
			stm.close();
			con.close();
		}
		catch(Exception e)
		{
			System.out.println(e);
		}
	}

	public Vector retNotDoneSubList(String sid)
	{
		Vector v = new Vector();
		System.out.println("* Client: Vector retNotDoneSubList("+sid+")");

		try
		{
			Class.forName("sun.jdbc.odbc.JdbcOdbcDriver");
			Connection con = DriverManager.getConnection("jdbc:odbc:db","","");
			Statement stm = con.createStatement();
			ResultSet rs = stm.executeQuery("SELECT sub FROM sub WHERE sid='"+sid+"' AND status='Not done' ORDER BY sub");
			while(rs.next())
				v.add(rs.getString("sub"));
			stm.close();
			con.close();
		}
		catch(Exception e)
		{
			System.out.println(e);
		}
		return v;
	}

	public Vector retTakenSubList(String sid)
	{
		Vector v = new Vector();
		System.out.println("* Client: Vector retTakenSubList("+sid+")");

		try
		{
			Class.forName("sun.jdbc.odbc.JdbcOdbcDriver");
			Connection con = DriverManager.getConnection("jdbc:odbc:db","","");
			Statement stm = con.createStatement();
			ResultSet rs = stm.executeQuery("SELECT sub FROM sub WHERE sid='"+sid+"' AND status='Taken' ORDER BY sub");
			while(rs.next())
				v.add(rs.getString("sub"));
			stm.close();
			con.close();
		}
		catch(Exception e)
		{
			System.out.println(e);
		}
		return v;
	}

	public boolean validUser(String user, String pass)
	{
		boolean res = false;
		System.out.println("* Client: boolean validUser("+user+", "+pass+")");

		try
		{
			Class.forName("sun.jdbc.odbc.JdbcOdbcDriver");
			Connection con = DriverManager.getConnection("jdbc:odbc:db","","");
			Statement stm = con.createStatement();
			ResultSet rs = stm.executeQuery("SELECT * FROM Users");
			while(rs.next())
				if(rs.getString("User").equals(user) && rs.getString("Pass").equals(pass))
					res = true;
			stm.close();
			con.close();
		}
		catch(Exception e)
		{
			System.out.println(e);
		}
		return res;
	}
}

public class Server
{
	public static void main(String args[])
	{
		try
		{
			ServerImpl obj = new ServerImpl();
			Naming.rebind("preAdvisingServer", obj);
		}
		catch(Exception e)
		{
			System.out.println(e);
		}
	}
}