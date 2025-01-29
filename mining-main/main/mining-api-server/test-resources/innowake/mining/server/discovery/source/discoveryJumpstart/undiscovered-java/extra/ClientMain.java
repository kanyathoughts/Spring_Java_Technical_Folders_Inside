import java.util.Vector;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class ClientMain extends JFrame implements ActionListener, KeyListener, ItemListener
{
	private JButton btnCancel;
	private JButton btnReg;
	private JButton btnRef;
	private JButton btnRem;
	private JList lstSub;
	private JButton btnAdd;
	private JComboBox cmbSubList;
	private JTextField txtID;
	private JLabel lblSubList;
	private JLabel lblStudID;
	private JLabel lblTaken;

	private ServerIntf rmiObj;
	private Vector takenSub;

	public ClientMain(ServerIntf rmiObj)
	{
		//Setting up JFrame (ClientMain)..
		super("preAdvising Client Main");
		Container con = getContentPane();
		con.setLayout(null);

		//Setting up JTextField (txtID)..
		txtID = new JTextField();
		txtID.setBounds(123, 40, 144, 21);
		con.add(txtID);

		//Setting up JButton (btnCancel)..
		btnCancel = new JButton("Cancel");
		btnCancel.setBounds(285, 325, 84, 25);
		con.add(btnCancel);

		//Setting up JButton (btnReg)..
		btnReg = new JButton("Register");
		btnReg.setEnabled(false);
		btnReg.setBounds(103, 325, 84, 25);
		con.add(btnReg);

		//Setting up JButton (btnRef)..
		btnRef = new JButton("Refresh");
		btnRef.setBounds(194, 325, 84, 25);
		con.add(btnRef);

		//Setting up JButton (btnRem)..
		btnRem = new JButton("Remove");
		btnRem.setEnabled(false);
		btnRem.setBounds(376, 143, 84, 25);
		con.add(btnRem);

		//Setting up JButton (btnAdd)..
		btnAdd = new JButton("Add");
		btnAdd.setEnabled(false);
		btnAdd.setBounds(285, 111, 84, 25);
		con.add(btnAdd);

		//Setting up JList (lstSub)..
		lstSub = new JList();
		JScrollPane splstSub = new JScrollPane(lstSub);
		splstSub.setLocation(51, 143);
		splstSub.setSize(318, 171);
		con.add(splstSub);

		//Setting up JComboBox (cmbSubList)..
		cmbSubList = new JComboBox();
		cmbSubList.setBounds(123, 71, 247, 21);
		con.add(cmbSubList);

		//Setting up JLabel (lblSubList)..
		lblSubList = new JLabel("Subject List:");
		lblSubList.setBounds(51, 75, 76, 17);
		con.add(lblSubList);

		//Setting up JLabel (lblStudID)..
		lblStudID = new JLabel("Student ID:");
		lblStudID.setBounds(55, 40, 92, 13);
		con.add(lblStudID);

		//Setting up JLabel (lblTaken)..
		lblTaken = new JLabel("Subjects taken in this semester:");
		lblTaken.setBounds(51, 127, 255, 17);
		con.add(lblTaken);

		this.rmiObj = rmiObj;

		cmbSubList.addItemListener(this);
		txtID.addKeyListener(this);
		btnAdd.addActionListener(this);
		btnReg.addActionListener(this);
		btnCancel.addActionListener(this);
		btnRef.addActionListener(this);
		btnRem.addActionListener(this);

		setSize(505, 408);
		setLocation(100, 100);
		setResizable(false);  // Set MAXIMIZE button and Resizing disabled
		setVisible(true);
	}

	public void itemStateChanged(ItemEvent e)
	{
		if(e.getStateChange()==1)
//			tmpSub = (String)e.getItem();
;
	}

	public void keyPressed(KeyEvent e)
	{
		Vector v = new Vector();
		if(e.getKeyCode()==KeyEvent.VK_ENTER)
		{
			try
			{
				v = rmiObj.retNotDoneSubList(txtID.getText());
				if(v.size()<1)
				{
					JOptionPane.showMessageDialog(null, "Incorrect ID.", "Warning!", JOptionPane.WARNING_MESSAGE);
					lstSub.setListData(new Vector());
					cmbSubList.removeAllItems();
					txtID.setText("");
					btnAdd.setEnabled(false);
					btnRem.setEnabled(false);
					btnReg.setEnabled(false);
				}

				lstSub.setListData(new Vector());
				cmbSubList.removeAllItems();

				for(int i=0; i<v.size(); ++i)
					cmbSubList.addItem(v.elementAt(i));

				if(v.size()>0)
					btnAdd.setEnabled(true);

				takenSub = rmiObj.retTakenSubList(txtID.getText());
				lstSub.setListData(takenSub);
				if(takenSub.size()>0)
				{
					btnRem.setEnabled(true);
					btnReg.setEnabled(true);
				}
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

		if(e.getSource()==btnRef)
		{
			txtID.setText("");
			cmbSubList.removeAllItems();
			lstSub.setListData(new Vector());
		}

		if(e.getSource()==btnAdd)
		{
			if(cmbSubList.getSelectedIndex()==-1)
			{
				JOptionPane.showMessageDialog(null, "You have to select a subject first to add.", "Warning!", JOptionPane.WARNING_MESSAGE);
				return;
			}
			for(int i=0; i<takenSub.size(); ++i)
				if(takenSub.get(i).toString().equals(cmbSubList.getSelectedItem().toString()))
				{
					JOptionPane.showMessageDialog(null, "You already have added this subject in the list.", "Warning!", JOptionPane.WARNING_MESSAGE);
					return;
				}
			takenSub.add(cmbSubList.getSelectedItem().toString());
			lstSub.setListData(takenSub);
			if(takenSub.size()>0)
				btnReg.setEnabled(true);
		}

		if(e.getSource()==btnReg)
		{
			if(txtID.getText().equals(""))
			{
				JOptionPane.showMessageDialog(null, "You have not entered student ID.", "Warning!", JOptionPane.WARNING_MESSAGE);
				return;
			}

			try
			{
				rmiObj.regSub(takenSub, txtID.getText());
			}
			catch(Exception ex)
			{
				System.out.println(ex);
			}
		}

		if(e.getSource()==btnRem)
		{
			if(lstSub.getSelectedIndex()==-1)
			{
				JOptionPane.showMessageDialog(null, "You have to select a subject first from the list to remove.", "Warning!", JOptionPane.WARNING_MESSAGE);
				return;
			}

			Object[] tmp = lstSub.getSelectedValues();
			for(int i=0; i<tmp.length; ++i)
				takenSub.removeElement(tmp[i].toString());
			lstSub.setListData(takenSub);
			if(takenSub.size()>0)
				btnReg.setEnabled(true);
		}
	}
}

