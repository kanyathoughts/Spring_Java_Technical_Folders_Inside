package innowake.mining.moduleextraction.natural;

public class RuleBeforeDefinineData {

	public static String apply(String t) {
		int defDataIndex = t.indexOf("DEFINE DATA");
		return t.substring(0, defDataIndex);
	}

}
