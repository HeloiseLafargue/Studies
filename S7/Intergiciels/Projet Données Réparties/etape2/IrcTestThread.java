import java.awt.*;
import java.awt.event.*;
import java.rmi.*;
import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.*;
import java.rmi.registry.*;


public class IrcTestThread extends Frame {
	public TextArea		text;
	public TextField	data;
	Sentence_itf		sentence;
	static String		myName;
	public boolean action_r = false;
	public boolean action_w = false;

	public static void main(String argv[]) {
		
		if (argv.length != 1) {
			System.out.println("java IrcTestThread <name>");
			return;
		}
		myName = argv[0];
	
		// initialize the system
		Client.init();
		
		// look up the IRCT object in the name server
		// if not found, create it, and register it in the name server
		Sentence_itf s = (Sentence_itf)Client.lookup("IRCT");
		if (s == null) {
			s = (Sentence_itf)Client.create(new Sentence());
			Client.register("IRCT", s);
		}
		// create the graphical part
		new IrcTestThread(s);
	}

	public IrcTestThread(Sentence_itf s) {
	
		setLayout(new FlowLayout());
	
		text=new TextArea(10,60);
		text.setEditable(false);
		text.setForeground(Color.red);
		add(text);
	
		data=new TextField(60);
		add(data);
	
		Button write_button = new Button("write");
		write_button.addActionListener(new WriteListenerThread(this, write_button));
		add(write_button);
		Button read_button = new Button("read");
		read_button.addActionListener(new ReadListenerThread(this, read_button));
		add(read_button);
		
		setSize(470,300);
		text.setBackground(Color.black); 
		show();
		
		sentence = s;
	}
}

class ReadListenerThread implements ActionListener {
	IrcTestThread irc;
	Button read_button;
	
	public ReadListenerThread(IrcTestThread i, Button b) {
		irc = i;
		read_button = b;
		
	}
	public void actionPerformed(ActionEvent e) {
		if (irc.action_r == false) {
			read_button.setLabel("arret");
			new ActionRead(irc).start();
		} else {
			irc.action_r = false;
			read_button.setLabel("read");
		}
	}
}

class ActionRead extends Thread {
	IrcTestThread irc;
	
	public ActionRead(IrcTestThread i) {
		irc = i;
	}
	public void run() {
		irc.action_r = true;
		while (irc.action_r == true){
			try {
				sleep(100);
			} catch (Exception e) {
				e.printStackTrace();
			}
			// lock the object in read mode
			irc.sentence.lock_read();
				
			// invoke the method
			String s = irc.sentence.read();
			
			// unlock the object
			irc.sentence.unlock();
			
			// display the read value
			irc.text.append(s+"\n");
		}
	}
}
		
	

class WriteListenerThread implements ActionListener {
	IrcTestThread irc;
	Button write_button;
	
	public WriteListenerThread(IrcTestThread i, Button w) {
        	irc = i;
        	write_button = w;
	}
	public void actionPerformed(ActionEvent e) {
		if (irc.action_w == false) {
			write_button.setLabel("arret");
			new ActionWrite(irc).start();
		} else {
			irc.action_w = false;
			write_button.setLabel("write");
		}
	}
}

class ActionWrite extends Thread {
		
	IrcTestThread irc;
	public ActionWrite(IrcTestThread i) {
		irc = i;
	}
	public void run() {
		irc.action_w = true;
		int compteur = 0;
		int nbmessage = 100;

		// get the value to be written from the buffer
		String s = irc.data.getText();
		while (irc.action_w == true && compteur<nbmessage){
			
			try {
				sleep(100);
			} catch (Exception e) {
				e.printStackTrace();
			}			
					
			// lock the object in write mode
			irc.sentence.lock_write();
			
			// invoke the method
			irc.sentence.write(IrcTestThread.myName+" wrote "+s);
			
			
			// unlock the object
			irc.sentence.unlock();
			
			
			compteur++;
		}
		irc.data.setText("");
	}
}

