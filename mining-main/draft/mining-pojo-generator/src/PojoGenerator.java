import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;

/*
	<class prefix>
	<+|*|- non-null(/selectable)/nullable> <field type> <name> [<JSON alias>[,<alias>]...]
 *
	Test
	+ String	name
	- Integer	value	theValue,someValue
 */
public class PojoGenerator {
	
	private static class Field {
		public boolean nullable;
		public boolean selectable;
		public String type;
		public String name;
		public String[] alias;
		
		public Field(String[] f) {
			if (f[0].length() > 0) {
				nullable = f[0].charAt(0) == '-';
				selectable = f[0].charAt(0) == '*';
			}
			type = f[1];
			name = f[2];
			if (f.length > 3) {
				alias = f[3].split(",");
			}
		}
	}
	
	private String cls = null;
	private final ArrayList<Field> fields = new ArrayList<>();
	private final PrintStream out;
	
	public PojoGenerator(final BufferedReader in, final PrintStream out) throws IOException {
		final ArrayList<String> lines = new ArrayList<>();
		while (true)  {
			String l = in.readLine();
			if (l.isBlank()) {
				break;
			}
			lines.add(l);
		}
		
		for (final var l : lines) {
			String fld = l.trim();
			if (cls == null) {
				cls = fld;
			} else {
				fields.add(new Field(fld.split("\\t+")));
			}
		}
		
		if (cls == null || fields.isEmpty()) {
			throw new IllegalStateException("Incomplete definition.");
		}
		this.out = out;
	}
	
	private void line(String s) {
		out.println(s);
	}
	
	private void linePart(String s) {
		out.print(s);
	}
	
	private static String initialUpper(String s) {
		return s.substring(0, 1).toUpperCase() + s.substring(1);
	}
	
	private void genPojo() {
		line("public class " + cls + "Pojo extends MiningPojo {\n\t");
		
		for (var fld : fields) {
			if (fld.nullable) {
				line("\t@Nullable");
			}
			line("\tprivate final " + (fld.selectable ? "Optional<" + fld.type + ">" : fld.type) + " " + fld.name + ";");
		}
		
		line("\t\n\tpublic " + cls + "Pojo(");
		line("\t\t\t@JsonProperty(\"uid\") final UUID uid,");
		linePart("\t\t\t@JsonProperty(\"nid\") final Long nid");
		for (var fld : fields) {
			linePart(",\n\t\t\t@JsonProperty(\"" + (fld.alias == null ? fld.name : fld.alias[0]) + "\")");
			if (fld.alias != null) {
				for (int n = 1; n < fld.alias.length; n++) {
					linePart(" @JsonAlias(\"" + fld.alias[n] + "\")");
				}
			}
			linePart((fld.nullable || fld.selectable ? " @Nullable" : "") + " final " + fld.type + " " + fld.name);
		}
		linePart(",\n\t\t\t@JsonProperty(\"customProperties\") final Map<String, Object> customProperties");
		line(") {\n\t\tsuper(new EntityId(uid, nid), customProperties);");
		for (var fld : fields) {
			line("\t\tthis." + fld.name + " = " + (fld.selectable ? "Optional.ofNullable(" + fld.name + ")" : fld.name) + ";");
		}
		
		line("\t}\n");
		
		for (var fld : fields) {
			if (fld.nullable) {
				line("\t@Nullable");
			}
			if (fld.alias != null && ! fld.name.equals(fld.alias[0])) {
				line("\t@JsonProperty(\"" + fld.alias[0] + "\")");
			}
			line("\tpublic " +  (fld.selectable ? "Optional<" + fld.type + ">"  : fld.type) + " get" + initialUpper(fld.name) + "() {");
			line("\t\treturn " + fld.name + ";");
			line("\t}\n\t");
		}
		
		line("}");
	}
	
	private void genProtoPojo() {
		line("public class " + cls + "PojoPrototype extends MiningPojoPrototype<" + cls + "PojoPrototype> {\n\t");
		
		for (var fld : fields) {
			if (fld.alias != null && ! fld.name.equals(fld.alias[0])) {
				line("\t@JsonProperty(\"" + fld.alias[0] + "\")");
			}
			line("\tpublic final Definable<" + fld.type + "> " + fld.name
					+" = new Definable<>(" + (fld.nullable ? "true" : "false") + ", \"" + cls + "." + fld.name + "\");");
		}
		
		line("\t");
		line("\tpublic " + cls + "PojoPrototype() {");
		line("\t\tsuper(\"" + cls + "\");");
		line("\t}\n\t");
		
		for (var fld : fields) {
			if (fld.alias != null) {
				if (! fld.name.equals(fld.alias[0])) {
					line("\t@JsonProperty(\"" + fld.alias[0] + "\")");
				}
				for (int n = 1; n < fld.alias.length; n++) {
					line("\t@JsonAlias(\"" + fld.alias[n] + "\")");
				}
			}
			line("\tpublic " + cls + "PojoPrototype set" + initialUpper(fld.name)
					+ "(" + (fld.nullable ? "@Nullable " : "") + "final " + fld.type  + " " + fld.name + ") {");
			line("\t\tthis." + fld.name + ".set(" + fld.name + ");");
			line("\t\treturn this;");
			line("\t}\n\t");
		}
		
		line("}");
	}
	
	private void genInstance() {
		line("\tnew " + cls + "Pojo(");
		
		line("\t\t/* uid (UUID) */");
		line("\t\t/* nid (Long) */");
		for (var fld : fields) {
			line("\t\t/* " + fld.name + " " + (fld.nullable ? "[" : "(") + fld.type + (fld.nullable ? "]" : ")") + " */");
		}
		line("\t\t/* customProperties (Map<String, Object>) */");
		
		line("\t)");
	}
	
	public static void main(String[] args) throws IOException {
		var generator = new PojoGenerator(new BufferedReader(new InputStreamReader(System.in)), System.out);
		generator.genPojo();
		System.out.println();
		generator.genProtoPojo();
		System.out.println();
		generator.genInstance();
	}
	
}
