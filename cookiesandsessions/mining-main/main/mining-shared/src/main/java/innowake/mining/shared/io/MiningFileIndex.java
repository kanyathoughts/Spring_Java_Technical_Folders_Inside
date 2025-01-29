package innowake.mining.shared.io;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Model class for a MiningFileIndex entity.
 */
public class MiningFileIndex implements Serializable {

	public static final String NAME = ".mining-file-index";

	/**
	 * Static inner class for a File.
	 */
	public static class File implements Serializable {

		@Nullable
		private Long id;

		private String path;

		@Nullable
		private Technology technology;

		@Nullable
		private Type type;

		private long metaDataRevision;

		private long contentRevision;
		
		private boolean contentIncluded;
		
		public File() {
			this.path = "";
			this.contentRevision = 1;
			this.metaDataRevision = 1;
		}

		/**
		 * Constructor.
		 * 
		 * @param id the id of the associated file
		 * @param path the path of the file
		 * @param technology the {@link Technology} of the file
		 * @param type the {@link Type} of the file
		 * @param metaDataRevision the value of meta data revision
		 * @param contentRevision the value of the content revision
		 * @param contentIncluded set to {@code true} if the Source code content is included. This is set only by the client
		 */
		public File(@Nullable final Long id, final String path, @Nullable final Technology technology, @Nullable final Type type,
				final long metaDataRevision, final long contentRevision, final boolean contentIncluded) {
			this.id = id;
			this.path = path;
			this.technology = technology;
			this.type = type;
			this.metaDataRevision = metaDataRevision;
			this.contentRevision = contentRevision;
			this.contentIncluded = contentIncluded;
		}

		/**
		 * Returns the file ID.
		 *
		 * @return the file ID
		 */
		@Nullable
		public Long getId() {
			return id;
		}
		
		/**
		 * Sets the file ID. 
		 *
		 * @param id the file ID to set
		 */
		public void setId(final Long id) {
			this.id = id;
		}

		/**
		 * Returns the path.
		 * 
		 * @return the path
		 */
		public String getPath() {
			return path;
		}
		
		/**
		 * Sets the file path. 
		 *
		 * @param path the file path to set
		 */
		public void setPath(final String path) {
			this.path = path;
		}

		/**
		 * Returns the {@link Technology}.
		 * 
		 * @return the {@link Technology}
		 */
		@Nullable
		public Technology getTechnology() {
			return technology;
		}
		
		/**
		 * Sets the {@link Technology}. 
		 *
		 * @param technology the {@link Technology} to set
		 */
		public void setTechnology(final Technology technology) {
			this.technology = technology;
		}

		/**
		 * Returns the {@link Type}.
		 * 
		 * @return the {@link Type}
		 */
		@Nullable
		public Type getType() {
			return type;
		}
		
		/**
		 * Sets the {@link Type}. 
		 *
		 * @param type the {@link Type} to set
		 */
		public void setType(final Type type) {
			this.type = type;
		}

		/**
		 * Returns the metaDataRevision.
		 *
		 * @return the metaDataRevision
		 */
		public long getMetaDataRevision() {
			return metaDataRevision;
		}
		
		/**
		 * Sets the metaDataRevision. 
		 *
		 * @param metaDataRevision the metaDataRevision to set
		 */
		public void setMetaDataRevision(final long metaDataRevision) {
			this.metaDataRevision = metaDataRevision;
		}

		/**
		 * Returns the contentRevision.
		 *
		 * @return the contentRevision
		 */
		public long getContentRevision() {
			return contentRevision;
		}
		
		/**
		 * Sets the contentRevision. 
		 *
		 * @param contentRevision the contentRevision to set
		 */
		public void setContentRevision(final long contentRevision) {
			this.contentRevision = contentRevision;
		}
		
		/**
		 * Sets all the parameters of File constructor.
		 * 	
		 * @param sourceObject sourceObject of file
		 * @return File object
		 */
		public static File setFromSourceObject(final SourcePojo sourceObject) {
			return new File(sourceObject.getId(), sourceObject.getPath(), sourceObject.getTechnology(), sourceObject.getType(),
					sourceObject.getMetaDataRevision().longValue(), sourceObject.getContentRevision().longValue(), false);
		}

		/**
		 * Denotes whether the content is included in the entry.
		 *
		 * @return {@code true} if content is included else {@code false}
		 */
		public boolean isContentIncluded() {
			return contentIncluded;
		}

		/**
		 * Sets the contentIncluded flag.
		 *
		 * @param contentIncluded Set to {@code true} if the content is included in the entry else {@code false}
		 */
		public void setContentIncluded(final boolean contentIncluded) {
			this.contentIncluded = contentIncluded;
		}

		@Override
		public int hashCode() {
			return Objects.hash(Long.valueOf(contentRevision), id, Long.valueOf(metaDataRevision), path, technology, type);
		}

		@Override
		public boolean equals(@Nullable Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			final File other = (File) obj;
			return contentRevision == other.contentRevision && Objects.equals(id, other.id) && metaDataRevision == other.metaDataRevision
					&& Objects.equals(path, other.path) && technology == other.technology && type == other.type;
		}

		@Override
		public String toString() {
			final ToStringBuilder builder = new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE);
			builder.append("id", id).append("path", path).append("technology", technology).append("type", type).append("metaDataRevision", metaDataRevision)
					.append("contentRevision", contentRevision);
			return builder.toString();
		}
	}

	private int version;

	private Long sourceCodeRevision;

	@Nullable
	private String scope;

	private List<File> files;
	
	/**
	 * Constructor.
	 */
	public MiningFileIndex() {
		this.sourceCodeRevision = Long.valueOf(1);
		this.files = new ArrayList<>();
	}

	/**
	 * Constructor.
	 * 
	 * @param version file version
	 * @param sourceCodeRevision revision number
	 * @param scope file scope
	 * @param files list of files
	 */
	public MiningFileIndex(final int version, final Long sourceCodeRevision, @Nullable final String scope, final List<File> files) {
		this.version = version;
		this.sourceCodeRevision = sourceCodeRevision;
		this.scope = scope;
		this.files = files;
	}

	/**
	 * Returns the file version.
	 * 
	 * @return the file version
	 */
	public int getVersion() {
		return version;
	}
	
	/**
	 * Sets the file file version. 
	 *
	 * @param version the version to set
	 */
	public void setVersion(final int version) {
		this.version = version;
	}
	
	/**
	 * Returns the sourceCode revision.
	 * 
	 * @return the sourceCode revision
	 */
	public Long getSourceCodeRevision() {
		return sourceCodeRevision;
	}

	/**
	 * Sets the sourceCode revision.
	 *
	 * @param sourceCodeRevision the sourceCode revision.
	 */
	public void setSourceCodeRevision(final Long sourceCodeRevision) {
		this.sourceCodeRevision = sourceCodeRevision;
	}

	/**
	 * Returns the file scope.
	 * 
	 * @return the file scope
	 */
	@Nullable
	public String getScope() {
		return scope;
	}
	
	/**
	 * Sets the file scope.
	 *
	 * @param scope the file scope.
	 */
	public void setScope(final String scope) {
		this.scope = scope;
	}

	/**
	 * Returns the list of files.
	 * 
	 * @return the list of files
	 */
	public List<File> getFiles() {
		return files;
	}

	/**
	 * Sets the files.
	 *
	 * @param files the source code files to upload.
	 */
	public void setFiles(final List<File> files) {
		this.files = files;
	}

	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE);
		builder
			.append("version", version)
			.append("sourceCodeRevision", sourceCodeRevision)
			.append("scope", scope)
			.append("files", files);
		return builder.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + files.hashCode();
		result = prime * result + ((scope == null) ? 0 : assertNotNull(scope).hashCode());
		result = prime * result + sourceCodeRevision.hashCode();
		result = prime * result + version;
		return result;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final MiningFileIndex other = (MiningFileIndex) obj;
		if ( ! files.equals(other.files)) {
			return false;
		}
		if (scope == null) {
			if (other.scope != null) {
				return false;
			}
		} else if ( ! assertNotNull(scope).equals(other.scope)) {
			return false;
		} else if ( ! sourceCodeRevision.equals(other.sourceCodeRevision)) {
			return false;
		} else if (version != other.version) {
			return false;
		}
		return true;
	}
}
