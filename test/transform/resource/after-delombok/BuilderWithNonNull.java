class BuilderWithNonNull {
	@lombok.NonNull
	private final String id;
	@java.lang.SuppressWarnings("all")
	BuilderWithNonNull(@lombok.NonNull final String id) {
		if (id == null) {
			throw new java.lang.NullPointerException("id is marked non-null but is null");
		}
		this.id = id;
	}
	@java.lang.SuppressWarnings("all")
	public static class BuilderWithNonNullBuilder {
		@java.lang.SuppressWarnings("all")
		private String id;
		@java.lang.SuppressWarnings("all")
		BuilderWithNonNullBuilder() {
		}
		@java.lang.SuppressWarnings("all")
		public BuilderWithNonNullBuilder id(@lombok.NonNull final String id) {
			if (id == null) {
				throw new java.lang.NullPointerException("id is marked non-null but is null");
			}
			this.id = id;
			return this;
		}
		@java.lang.SuppressWarnings("all")
		public BuilderWithNonNull build() {
			return new BuilderWithNonNull(id);
		}
		@java.lang.Override
		@java.lang.SuppressWarnings("all")
		public java.lang.String toString() {
			return "BuilderWithNonNull.BuilderWithNonNullBuilder(id=" + this.id + ")";
		}
	}
	@java.lang.SuppressWarnings("all")
	public static BuilderWithNonNullBuilder builder() {
		return new BuilderWithNonNullBuilder();
	}
}