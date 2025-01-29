import java.rmi.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class ClientLogin extends JFrame implements ActionListener, KeyListener
{
	private JButton btnCancel;
	private JButton btnOK;
	private JPasswordField txtPass;
	private JTextField txtName;
	private JLabel lblPass;
	private JLabel lblName;
	private static ServerIntf rmiObj;

	public ClientLogin()
	{
		//Setting up JFrame (ClientLogin)..
		super("preAdvising Client Login");
		Container con = getContentPane();
		con.setLayout(null);

		//Setting up JButton (btnCancel)..
		btnCancel = new JButton("Cancel");
		btnCancel.setBounds(154, 123, 84, 25);
		con.add(btnCancel);

		//Setting up JButton (btnOK)..
		btnOK = new JButton("OK");
		btnOK.setBounds(63, 123, 84, 25);
		con.add(btnOK);

		//Setting up JPasswordField (txtPass)..
		txtPass = new JPasswordField();
		txtPass.setBounds(95, 79, 144, 21);
		con.add(txtPass);

		//Setting up JTextField (txtName)..
		txtName = new JTextField();
		txtName.setBounds(95, 44, 144, 21);
		con.add(txtName);

		//Setting up JLabel (lblPass)..
		lblPass = new JLabel("Password:");
		lblPass.setBounds(24, 79, 64, 17);
		con.add(lblPass);

		//Setting up JLabel (lblName)..
		lblName = new JLabel("Name:");
		lblName.setBounds(51, 44, 60, 17);
		con.add(lblName);

		txtName.addKeyListener(this);
		txtPass.addKeyListener(this);
		btnOK.addActionListener(this);
		btnCancel.addActionListener(this);

		setSize(277, 210);
		setLocation(100, 100);
		setResizable(false);  // Set MAX button and Resizing disabled
		setVisible(true);
	}

	public void keyPressed(KeyEvent e)
	{
		if(e.getKeyCode()==KeyEvent.VK_ENTER)
		{
			String u = txtName.getText();
			String p = new String(txtPass.getPassword());

			if(u.equals("") || p.equals(""))
			{
				JOptionPane.showMessageDialog(null, "Type username and password.", "Warning!", JOptionPane.WARNING_MESSAGE);
				return;
			}

			try
			{
				if(rmiObj.validUser(u, p))
				{
					ClientMain cm = new ClientMain(rmiObj);
					cm.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
					setVisible(false);
				}
				else
					JOptionPane.showMessageDialog(null, "Invalid username and/or password.", "Error!", JOptionPane.WARNING_MESSAGE);
			}
			catch(Exception ex)
			{
				System.out.println(ex);
			}
		}
	}

 	public void keyReleased(KeyEvent e) {}
	public void keyTyped(KeyEvent e) {}

	public void actionPerformed(ActionEvent e)
	{
		if(e.getSource()==btnCancel)
			System.exit(0);
		if(e.getSource()==btnOK)
		{
			String u = txtName.getText();
			String p = new String(txtPass.getPassword());

			if(u.equals("") || p.equals(""))
			{
				JOptionPane.showMessageDialog(null, "Type username and password.", "Warning!", JOptionPane.WARNING_MESSAGE);
				return;
			}

			try
			{
				if(rmiObj.validUser(u, p))
				{
					ClientMain cm = new ClientMain(rmiObj);
					cm.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
					setVisible(false);
				}
				else
					JOptionPane.showMessageDialog(null, "Invalid username and/or password.", "Error!", JOptionPane.WARNING_MESSAGE);
			}
			catch(Exception ex)
			{
				System.out.println(ex);
			}
		}
	}

	public static void main(String args[])
	{
		try
		{
			ClientLogin app = new ClientLogin();
			app.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

			rmiObj = (ServerIntf)Naming.lookup("rmi://"+args[0]+"/preAdvisingServer");
		}
		catch(Exception e)
		{
			System.out.println(e);
		}
	}
}


