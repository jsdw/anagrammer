import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class AnagramFinderBasic {
	private static Pattern linePattern = Pattern.compile("[^a-zA-Z ]+");
	private static Pattern whitespacePattern = Pattern.compile("\\s+");

	// We use a linked hash map to maintain the order that things were added
	private Map<String, Map<String, String>> lineMap = new LinkedHashMap<>();

	/**
	 * Processes the passed line and adds it to the Anagram Finder.
	 *
	 * @param line the line of text to add.
	 */
	public void processAndAddLine(String line) {
		// short-circuit if empty line
		if (line.length() == 0) {
			return;
		}

		String sanitisedLine = linePattern.matcher(line.trim().toLowerCase()).replaceAll("");

		String[] wordList = whitespacePattern.split(sanitisedLine);
		String justCharacters = String.join("", wordList);

		// if line contains no characters, let's ignore it.
		if (justCharacters.length() == 0) {
			return;
		}

		// Generate a key for the line, based on the list of characters sorted
		char[] lineChars = justCharacters.toCharArray();
		Arrays.sort(lineChars);
		String lineKey = new String(lineChars);

		// Generate a key for the version of the line so that we can ignore lines made up of the same words (ignoring the
		// order of the words in the line). For part 1, wordKey would just have been sanitisedLine.
		Arrays.sort(wordList);
		String wordKey = String.join(" ", wordList);

		// If we've encountered a key for the first time, then we create the entry in the map
		if (!lineMap.containsKey(lineKey)) {
			lineMap.put(lineKey, new HashMap<>());
		}

		Map<String, String> wordMap = lineMap.get(lineKey);

		// If this version of the anagram is not already in the map, then add it
		if (!wordMap.containsKey(wordKey)) {
			wordMap.put(wordKey, line);
		}
	}

	/**
	 * Get the list of matched anagrams, each of which is a list of the versions of the anagram encountered.
	 *
	 * @return the list of matched anagrams.
	 */
	public Collection<Collection<String>> getMatches() {
		return lineMap.values().stream().filter(p -> p.size() > 1)
		                                .map(Map::values)
		                                .collect(Collectors.toList());
	}

	public static void main(String[] args) {
		if (args.length != 1) {
			System.err.println("Usage: java AnagramFinderBasic <inputFile>");
			System.exit(1);
		}

		AnagramFinderBasic af = new AnagramFinderBasic();

		// load content from file
		String fileName = args[0];
		try (Stream<String> lineStream = Files.lines(Paths.get(fileName))) {
			lineStream.forEach(af::processAndAddLine);
		} catch (IOException e) {
			System.err.println("Disaster!");
			e.printStackTrace();
			System.exit(2);
		}

		// iterate over response and output
		Collection<Collection<String>> matches = af.getMatches();

		for (Collection<String> match : matches) {
			for (String version : match) {
				System.out.println(version);
			}
			System.out.println();
		}

		System.out.printf("There were %d %s%n", matches.size(), (matches.size() == 1 ? "match" : "matches"));
	}
}
