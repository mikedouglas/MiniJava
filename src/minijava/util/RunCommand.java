package minijava.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * A utility to run a system command. 
 * <p>
 * I know this works under unix. It probably doesn't work as is under windows.
 */
public class RunCommand {
	
	private int returnCode;
	private String output;
	private String errorOutput;
	
	/**
	 * Start a process to run a command and wait for it to complete. The created instance will
	 * store the resultcode, and can optionally also keep the contents of the output and error
	 * streams.
	 * 
	 * @param cmd
	 * @param wantOutput
	 * @param wantErrorOutput
	 * @throws IOException 
	 */
	public RunCommand(String[] cmd, boolean wantOutput, boolean wantErrorOutput) throws IOException {
		Runtime rt = Runtime.getRuntime();
		Process process = rt.exec(cmd);
		StreamGobbler errorGobbler = new StreamGobbler(process.getErrorStream(), wantErrorOutput);
		StreamGobbler outputGobbler = new StreamGobbler(process.getInputStream(), wantOutput);
		errorGobbler.start();
		outputGobbler.start();
		try {
			this.returnCode = process.waitFor();
			outputGobbler.join(); // Must do this or we may loose some output!
			errorGobbler.join();  // Must do this or we may loose some error output!
		} catch (InterruptedException e) {
			throw new Error(e); // I don't think this should happen.
		}
		if (wantOutput) 
			output = outputGobbler.keepIt.toString();
		if (wantErrorOutput) 
			errorOutput = errorGobbler.keepIt.toString();
	}

	public String getOutput() {
		return output;
	}
	public String getErrorOutput() {
		return errorOutput;
	}
	public int getReturnCode() {
		return returnCode;
	}

	/**
	 * We need Threads to consume the output of the command (from its error and output
	 * streams), otherwise, 
	 * if the command's process produces more output than can fit in the Stream's buffer,
	 * it will block.
	 */
	static class StreamGobbler extends Thread {
		
		private InputStream is;
		private StringBuilder keepIt;

		StreamGobbler(InputStream is, boolean wantOutput) {
			this.is = is;
			this.keepIt = wantOutput ? new StringBuilder() : null;
		}

		public void run() {
			try {
				InputStreamReader isr = new InputStreamReader(is);
				BufferedReader br = new BufferedReader(isr);
				String line=null;
				while ( (line = br.readLine()) != null) {
					if (keepIt!=null) {
						keepIt.append(line);
						keepIt.append(System.getProperty("line.separator"));
					}
				}
			} catch (IOException ioe) {
				ioe.printStackTrace();  
			}
		}
	}

}
