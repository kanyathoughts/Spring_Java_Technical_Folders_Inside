package fw2.orm.xlsx.io.mapper;

public class MappingException extends RuntimeException {

	public MappingException() {
		super();
		
	}

	public MappingException(String arg0, Throwable arg1, boolean arg2, boolean arg3) {
		super(arg0, arg1, arg2, arg3);
	
	}

	public MappingException(String arg0, Throwable arg1) {
		super(arg0, arg1);
	
	}

	public MappingException(String arg0) {
		super(arg0);
		
	}

	public MappingException(Throwable arg0) {
		super(arg0);
		
	}

	private static final long serialVersionUID = 6002817528048662321L;

}
