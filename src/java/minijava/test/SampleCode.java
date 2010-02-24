package minijava.test;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Arrays;

/**
 * We use this class as a place to put some methods and constants to get at
 * the sample code in the sample directory.
 * 
 * @author kdvolder
 */
public class SampleCode {

	/**
	 * Points to a directory containing sample java code to parse.
	 */
	public final static File sample_dir = new File("resources/sample");

	/**
	 * Filter for selecting Java files only.
	 */
	private static final FilenameFilter javaFileFilter = new FilenameFilter() {
		@Override
		public boolean accept(File dir, String name) {
			return name.endsWith(".java");
		}
	};

	/**
	 * @return An array of sample MiniJava files.
	 */
	public static File[] sampleFiles() {
		File[] files = sample_dir.listFiles(javaFileFilter);
		// Should sort the array to ensure that we produce the files in the same order
		// independent of the order in which the OS produces them.
		Arrays.sort(files);
		return files;
	}

}
