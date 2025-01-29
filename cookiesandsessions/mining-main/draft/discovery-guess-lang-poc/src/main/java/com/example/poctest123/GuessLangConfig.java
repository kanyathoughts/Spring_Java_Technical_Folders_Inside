package com.example.poctest123;

import java.util.HashMap;

public class GuessLangConfig {

	private int confidenceInterval = 20;
	private HashMap<String, String> fileExtensions;
	public GuessLangConfig() {
		fileExtensions = new HashMap<>();
		fileExtensions.put("Assembly", ".asm");
		fileExtensions.put(	"Batchfile", ".bat");
		fileExtensions.put(	"C", ".c");
		fileExtensions.put(	"C++", ".pp");
		fileExtensions.put(	"Clojure", ".clj");
		fileExtensions.put(	"CMake", ".cmake");
		fileExtensions.put(	"COBOL", ".cbl");
		fileExtensions.put(	"CoffeeScript", ".coffee");
		fileExtensions.put(	"CSS", ".css");
		fileExtensions.put(	"CSV", ".csv");
		fileExtensions.put(	"Dart", ".dart");
		fileExtensions.put(	"DM", ".dm");
		fileExtensions.put(	"Dockerfile", ".dockerfile");
		fileExtensions.put(	"Elixir", ".ex");
		fileExtensions.put(	"Erlang", ".erl");
		fileExtensions.put(	"Fortran", ".f90");
		fileExtensions.put(	"Go", ".go");
		fileExtensions.put(	"Groovy", ".grt");
		fileExtensions.put(	"Haskell", ".hs");
		fileExtensions.put(	"HTML", ".html");
		fileExtensions.put(	"INI", ".ini");
		fileExtensions.put(	"Java", ".java");
		fileExtensions.put(	"JavaScript", ".js");
		fileExtensions.put(	"JSON", ".json");
		fileExtensions.put(	"Julia", ".jl");
		fileExtensions.put(	"Kotlin", ".kt");
		fileExtensions.put(	"Lisp", ".lisp");
		fileExtensions.put(	"Lua", ".lua");
		fileExtensions.put(	"Makefile", ".mkfile");
		fileExtensions.put(	"Markdown", ".markdown");
		fileExtensions.put(	"Matlab", ".matlab");
		fileExtensions.put(	"Objective-C", ".c");
		fileExtensions.put(	"OCaml", ".mll");
		fileExtensions.put(	"Pascal", ".pas");
		fileExtensions.put(	"Perl", ".ph");
		fileExtensions.put(	"PHP", ".php");
		fileExtensions.put(	"PowerShell", ".ps1");
		fileExtensions.put(	"Prolog", ".prolog");
		fileExtensions.put(	"Python", ".py");
		fileExtensions.put(	"R", ".rd");
		fileExtensions.put(	"Ruby", ".rb");
		fileExtensions.put(	"Rust", ".rs.in");
		fileExtensions.put(	"Scala", ".scala");
		fileExtensions.put(	"Shell", ".sh");
		fileExtensions.put(	"SQL", ".cql");
		fileExtensions.put(	"Swift", ".swift");
		fileExtensions.put(	"TeX", ".tex");
		fileExtensions.put(	"TOML", ".toml");
		fileExtensions.put(	"TypeScript", ".ts");
		fileExtensions.put(	"Verilog", ".veo");
		fileExtensions.put(	"Visual", ".vb");
		fileExtensions.put(	"Basic", ".vb");
		fileExtensions.put(	"XML", ".xml");
		fileExtensions.put(	"YAML", ".yml");

	}


	public int getConfidenceInterval() {
		return confidenceInterval;
	}

	public void setConfidenceInterval(int confidenceInterval) {
		if(confidenceInterval <=100 && confidenceInterval >=0 ){
			this.confidenceInterval = confidenceInterval;
		}
	}

	public String getFileExtension(String fileType){
		return fileExtensions.get(fileType);
	}

}
