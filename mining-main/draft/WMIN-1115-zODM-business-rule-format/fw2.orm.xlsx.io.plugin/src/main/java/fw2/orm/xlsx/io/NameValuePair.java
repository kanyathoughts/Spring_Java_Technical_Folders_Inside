package fw2.orm.xlsx.io;

import java.util.ArrayList;

public class NameValuePair {
	public static boolean isEmpty(final ArrayList<NameValuePair> logicalRow) {

		for (final NameValuePair pair : logicalRow) {
			if (!pair.isEmpty()) {
				return false;
			}
		}
		return true;
	}

	String name;
	String value;

	public NameValuePair() {
		this.name = "";
		this.value = "";
	}

	public NameValuePair(final String name, final String value) {
		super();
		this.name = name;
		this.value = value;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if ((obj == null) || (obj.getClass() != this.getClass())) {
			return false;
		}
		// object must be NameValuePair at this point
		final NameValuePair test = (NameValuePair) obj;
		return ((this.name != null) && this.name.equals(test.name)) && ((this.value != null) && this.value.equals(test.value));
	}

	public String getName() {
		return this.name;
	}

	public String getValue() {
		return this.value;
	}

	@Override
	public int hashCode() {
		int hash = 7;
		hash = (31 * hash) + (null == this.name ? 0 : this.name.hashCode());
		hash = (31 * hash) + (null == this.value ? 0 : this.value.hashCode());
		return hash;
	}

	public boolean isEmpty() {
		return this.value == null;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public void setValue(final String value) {
		this.value = value;
	}
}
