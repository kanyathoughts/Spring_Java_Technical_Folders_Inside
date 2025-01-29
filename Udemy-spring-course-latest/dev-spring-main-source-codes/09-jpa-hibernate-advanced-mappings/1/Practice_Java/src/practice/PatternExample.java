package practice;

public class PatternExample {
	public static void main(String[] args) {
		/* for (int i = 0; i < 5; i++) {
			for (int j = 5; j > i; j--) {
				System.out.print("*");
			}
			System.out.println();
			
		} */
		
		String path = "https://gitlab.consulting.sltc.com/appmod/qef/infrastructure/mining-db-dumps.git";
		String[] strArray = path.split("//")[1].split("/");
		System.out.println("folderName is : " + path.split("//")[1].split("/")[strArray.length - 1].split(".git")[0]);	
	}

}
