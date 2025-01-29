/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.java;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.server.discovery.parser.sql.SqlUtil.getStoredProcedureNameFromCall;
import static innowake.mining.shared.model.ModuleType.JAVA_ANNOTATION;
import static innowake.mining.shared.model.ModuleType.JAVA_ENUM;
import static innowake.mining.shared.model.ModuleType.JAVA_INTERFACE;
import static innowake.mining.shared.model.ModuleType.JAVA_TYPE;

import java.sql.CallableStatement;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.contributors.AstNodeLocationProvider;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.ndt.core.parsing.spi.Document;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionMethodReference;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.MarkerAnnotation;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.QualifiedType;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleMemberAnnotation;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SuperMethodReference;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.dawn.metrics.contributors.java.JavaAstVisitor.Dependency.DependencyType;
import innowake.mining.server.discovery.parser.AstVisitor;
import innowake.mining.shared.model.ModuleType;
import innowake.ndt.parsing.parser.java.model.JavaModel;
import org.eclipse.jdt.core.dom.TypeMethodReference;

/**
 * An {@link AstVisitor} for {@link JavaModel JavaModels} that collect all required information for the {@link JavaContributor}.
 */
class JavaAstVisitor extends ASTVisitor {

	private static final Logger LOG = LoggerFactory.getLogger(JavaAstVisitor.class);
	private static final String METHOD_PREPARE_CALL = "prepareCall";
	private static final String[] EMPTY_ARR = { };

	private final List<JavaType> javaTypes = new ArrayList<>();
	private final List<StoredProcedureInvocation> storedProcedureInvocations = new ArrayList<>();
	private final List<Dependency> dependencies = new ArrayList<>();
	/* queue that holds the current type (class, enum, interface, annotation) to prevent that a type gets a dependency for itself */
	private final Deque<String> typeDeclarations = new ArrayDeque<>();
	private final Set<String> processedMethods = new HashSet<>();
	private final boolean collectMethods;
	private final List<MethodCall> methodCalls = new ArrayList<>();
	private final List<Tuple2<String, AstNodeLocation>> errors = new ArrayList<>();
	private final Document document;

	public JavaAstVisitor(final boolean collectMethods, final String sourceContent) {
		this.collectMethods = collectMethods;
		this.document = new Document(sourceContent);
	}

	/**
	 * @return The list of all found {@link JavaType JavaTypes} declarations
	 */
	public List<JavaType> getJavaTypes() {
		return javaTypes;
	}

	/**
	 * @return The list of all found {@link StoredProcedureInvocation StoredProcedureInvocations}
	 */
	@SuppressWarnings("javadoc")
	public List<StoredProcedureInvocation> getStoredProcedureInvocations() {
		return storedProcedureInvocations;
	}

	/**
	 * @return The list of all found {@link Dependency Dependencies}
	 */
	@SuppressWarnings("javadoc")
	public List<Dependency> getDependencies() {
		return dependencies;
	}

	/**
	 *
	 * @return the list of all the method calls found. Is empty if #collectMethods is false
	 */
	public List<MethodCall> getMethodCalls() {
		return methodCalls;
	}

	/**
	 *
	 * @return the errors occurred while collecting data.
	 */
	public List<Tuple2<String, AstNodeLocation>> getErrors() {
		return errors;
	}

	@Override
	public boolean visit(@Nullable final MethodInvocation node) {
		if (node != null) {
			/* Check if method returns an instance of java.sql.CallableStatement */
			if (isSqlCallableStatement(node)) {
				storedProcedureInvocations.add(new StoredProcedureInvocation(node, isSqlPrepareCall(node)));
			} else {
				final IMethodBinding methodBinding = node.resolveMethodBinding();
				/* Handle static method invoked by class */
				if (methodBinding != null && (methodBinding.getModifiers() & Modifier.STATIC) > 0
						&& ! (node.getExpression() instanceof ClassInstanceCreation)) {
					final ITypeBinding typeBinding = methodBinding.getDeclaringClass();
					if (typeBinding != null) {
						String qualifiedName = typeBinding.isParameterizedType()
								? typeBinding.getErasure().getQualifiedName()
								: typeBinding.getQualifiedName();
						qualifiedName = qualifiedName.isEmpty() ? typeBinding.getName() : qualifiedName;
						addDependencyChecked(node, qualifiedName, getPackageNameSafe(typeBinding),
								DependencyType.REFERENCE);
					}
				}
				if (collectMethods && methodBinding != null) {
					visitMethodInvocation(node, methodBinding, node.getName().getIdentifier());
				}
			}
		} else {
			LOG.debug("MethodBinding is not present for the MethodInvocation node" , node);
		}
		return true;
	}

	@Override
	public boolean visit(@Nullable final MethodDeclaration node) {
		if (node != null && collectMethods) {
			final var methodName = node.getName().toString();
			final var qualifiedClassName = getQualifiedClassName(node);

			if (qualifiedClassName.isPresent()) {
				/* We collect every method only once and ignore method overloading here */
				final String qualifiedMethodName = qualifiedClassName.get() + "." + methodName;
				if (processedMethods.add(qualifiedMethodName)) {
					javaTypes.add(new JavaType(node, ModuleType.JAVA_METHOD, qualifiedMethodName));
				}
			} else {
				final var lineNumbers = AstNodeLocationProvider.getLineNumbers(document, node.getStartPosition(), node.getLength());
				final var error = new Tuple2<>(String.format("Unable to collect method: %s at offset %d. Type binding for declaring class can not be resolved",
						methodName, Integer.valueOf(node.getStartPosition())), new AstNodeLocation(node.getStartPosition(), 0, node.getStartPosition()
						, 0, node.getStartPosition(), 0, lineNumbers.e1, lineNumbers.e2));
				errors.add(error);
			}
		}
		return super.visit(node);
	}

	@Override
	public boolean visit(@Nullable final SuperMethodReference node) {
		if (collectMethods) {
			final SuperMethodReference node2 = assertNotNull(node);
			final IMethodBinding methodBinding = node2.resolveMethodBinding();
			if (methodBinding != null) {
				visitMethodInvocation(node2, methodBinding, node2.getName().getIdentifier());
			}
		}

		return super.visit(node);
	}

	@Override
	public boolean visit(@Nullable final ExpressionMethodReference node) {
		/* this::methodod63 */
		if (collectMethods) {
			final ExpressionMethodReference node2 = assertNotNull(node);
			final IMethodBinding methodBinding = node2.resolveMethodBinding();
			if (methodBinding != null) {
				visitMethodInvocation(node2, methodBinding, node2.getName().getIdentifier());
			}
		}

		return super.visit(node);
	}

	@Override
	public boolean visit(@Nullable final TypeMethodReference node) {
		if (collectMethods) {
			final TypeMethodReference node2 = assertNotNull(node);
			final IMethodBinding methodBinding = node2.resolveMethodBinding();
			if (methodBinding != null) {
				visitMethodInvocation(node2, methodBinding, node2.getName().getIdentifier());
			}
		}

		return super.visit(node);
	}

	@Override
	public boolean visit(@Nullable final SuperMethodInvocation node) {
		if (collectMethods) {
			final SuperMethodInvocation node2 = assertNotNull(node);
			final IMethodBinding methodBinding = node2.resolveMethodBinding();
			if (methodBinding != null) {
				visitMethodInvocation(node2, methodBinding, node2.getName().getIdentifier());
			}
		}

		return super.visit(node);
	}


	private void visitMethodInvocation(final ASTNode node, final IMethodBinding methodBinding, final String methodName) {
		ITypeBinding typeBinding = methodBinding.getDeclaringClass();
		if (typeBinding != null) {
			String calleeType = getQualifiedClassName(typeBinding);

			/* qualifiedName is empty for anonymous classes */
			if (calleeType.isEmpty()) {
				typeBinding = typeBinding.getDeclaringClass();
				if (typeBinding != null) {
					calleeType = getQualifiedClassName(typeBinding);
				}
			}
				final String[] caller = getMethodCaller(node);
				final String[] parameters = getMethodParameters(methodBinding);
			    final var packageName = getPackageNameSafe(Objects.requireNonNull(typeBinding));
				methodCalls.add(new MethodCall(node, caller[0], caller[1], methodName, calleeType, parameters, packageName));
			}
	}

	private static String[] getMethodParameters(final IMethodBinding methodBinding) {
		final ITypeBinding[] parameterTypes = methodBinding.getParameterTypes();
		final String[] parameters;
		if (parameterTypes.length == 0) {
			parameters = EMPTY_ARR;
		} else {
			parameters = new String[parameterTypes.length];
			for (int i = 0; i < parameterTypes.length; i++) {
				parameters[i] = getQualifiedClassName(parameterTypes[i]);
			}
		}
		return parameters;
	}

	private String[] getMethodCaller(final ASTNode node) {
		ASTNode parent = node.getParent();

		while (parent != null) {
			if (parent instanceof AbstractTypeDeclaration) {
				final ITypeBinding typeBinding = ((AbstractTypeDeclaration) parent).resolveBinding();
				if (typeBinding != null) {
					return new String[] { typeBinding.getName(), getQualifiedClassName(typeBinding) };

				}
			} else if (parent instanceof MethodDeclaration) {
				final MethodDeclaration md = (MethodDeclaration) parent;
				if ( ! (md.getParent() instanceof AnonymousClassDeclaration)) {
					final IMethodBinding methodBinding = md.resolveBinding();
					if (methodBinding != null) {
						final ITypeBinding typeBinding = methodBinding.getDeclaringClass();
						if (typeBinding != null) {
							return new String[] { md.getName().getIdentifier(), getQualifiedClassName(typeBinding) };
						}
					}
				}
			}

			parent = parent.getParent();
		}

		if (node instanceof MethodInvocation) {
			throw new UnsupportedOperationException("Unhandled caller of method: " + ((MethodInvocation) node).getName().getIdentifier());
		} else if (node instanceof SuperMethodInvocation) {
			throw new UnsupportedOperationException("Unhandled caller of super method: " + ((SuperMethodInvocation) node).getName().getIdentifier());
		} else {
			throw new UnsupportedOperationException("Unhandled caller of caller: " + node);
		}
	}

	private static Optional<String> getQualifiedClassName(final MethodDeclaration node) {
		final IMethodBinding methodBinding = node.resolveBinding();
		if (methodBinding != null) {
			final ITypeBinding binding = methodBinding.getDeclaringClass();
			if (binding != null) {
				final String name = getQualifiedClassName(binding);
				/* name is empty for anonymous inner classes */
				if (StringUtils.isNoneBlank(name)) {
					return Optional.of(name);
				}
			}
		}

		/* For proper handling of methods in anonymous classes */
		ASTNode parent = node.getParent();
		while (parent != null) {
			if (parent instanceof TypeDeclaration) {
				final ITypeBinding typeBinding = ((TypeDeclaration) parent).resolveBinding();
				if (typeBinding != null) {
					return Optional.ofNullable(getQualifiedClassName(typeBinding));
				}
			}
			parent = parent.getParent();
		}
		return Optional.empty();
	}

	static String getQualifiedClassName(final ITypeBinding typeBinding) {
		return typeBinding.isParameterizedType() ? typeBinding.getErasure().getQualifiedName() : typeBinding.getQualifiedName();
	}

		private static boolean isSqlCallableStatement(final MethodInvocation node) {
		final ITypeBinding typeBinding = node.resolveTypeBinding();
		return typeBinding != null && isType(typeBinding, java.sql.CallableStatement.class.getName());
	}

	private static boolean isSqlPrepareCall(final MethodInvocation node) {
		final Expression expression = node.getExpression();
		if (expression != null) {
			final ITypeBinding  typeBinding = expression.resolveTypeBinding();
			return typeBinding != null
						&& isType(typeBinding, java.sql.Connection.class.getName())
						&& METHOD_PREPARE_CALL.equals(node.getName().getFullyQualifiedName());
		}

		return false;
	}

	private static boolean isType(final ITypeBinding typeBinding, final String className) {
		if (className.equals(typeBinding.getQualifiedName())) {
			return true;
		}

		final ITypeBinding superClassBinding = typeBinding.getSuperclass();
		if (superClassBinding != null && isType(superClassBinding, className)) {
			return true;
		}

		for (ITypeBinding binding : typeBinding.getInterfaces()) {
			if (isType(binding, className)) {
				return true;
			}
		}

		return false;
	}

	@Override
	public boolean visit(@Nullable final EnumDeclaration node) {
		if (node != null) {
			javaTypes.add(new JavaType(node, JAVA_ENUM));
			pushTypeDeclaration(typeDeclarations, node);
		}
		return true;
	}

	@Override
	public void endVisit(@Nullable final EnumDeclaration node) {
		if (node != null) {
			popTypeDeclaration(typeDeclarations, node);
		}
	}

	@Override
	public boolean visit(@Nullable final TypeDeclaration node) {
		if (node != null) {
			javaTypes.add(new JavaType(node, node.isInterface() ? JAVA_INTERFACE : JAVA_TYPE));
			pushTypeDeclaration(typeDeclarations, node);
		}

		return true;
	}

	@Override
	public void endVisit(@Nullable final TypeDeclaration node) {
		if (node != null) {
			popTypeDeclaration(typeDeclarations, node);
		}
	}

	@Override
	public boolean visit(@Nullable final AnnotationTypeDeclaration node) {
		if (node != null) {
			javaTypes.add(new JavaType(node, JAVA_ANNOTATION));
			pushTypeDeclaration(typeDeclarations, node);
		}
		return true;
	}

	@Override
	public void endVisit(@Nullable final AnnotationTypeDeclaration node) {
		popTypeDeclaration(typeDeclarations, assertNotNull(node));
	}

	private static void pushTypeDeclaration(final Deque<String> typeDeclarations, final AbstractTypeDeclaration node) {
		final ITypeBinding typeBinding = node.resolveBinding();
		if (typeBinding != null) {
			typeDeclarations.push(typeBinding.getQualifiedName());
		}
	}

	private static void popTypeDeclaration(final Deque<String> typeDeclarations, final AbstractTypeDeclaration node) {
		final ITypeBinding typeBinding = node.resolveBinding();
		if (typeBinding != null) {
			typeDeclarations.pop();
		}
	}

	@Override
	public boolean visit(@Nullable final NormalAnnotation node) {
		if (node != null) {
			handleAnnotation(node);
		}
		return true;
	}

	@Override
	public boolean visit(@Nullable final MarkerAnnotation node) {
		if (node != null) {
			handleAnnotation(node);
		}
		return true;
	}

	@Override
	public boolean visit(@Nullable final SingleMemberAnnotation node) {
		if (node != null) {
			handleAnnotation(node);
		}
		return true;
	}

	private void handleAnnotation(final Annotation annotation) {
		final ITypeBinding typeBinding = annotation.resolveTypeBinding();
		/* type binding is available if annotation is inside the classpath */
		if (typeBinding != null) {
			String qualifiedName = typeBinding.isParameterizedType() ? typeBinding.getErasure().getQualifiedName() : typeBinding.getQualifiedName();
			qualifiedName = qualifiedName.isEmpty() ? typeBinding.getName() : qualifiedName;
			addDependencyChecked(annotation, qualifiedName, getPackageNameSafe(typeBinding), DependencyType.ANNOTATION);
		}
	}

	@Override
	public boolean visit(@Nullable final SimpleType node) {
		if (node != null) {
			visitType(node);
		}
		return true;
	}

	@Override
	public boolean visit(@Nullable final QualifiedType node) {
		if (node != null) {
			visitType(node);
		}
		return true;
	}

	@Override
	public boolean visit(@Nullable final QualifiedName node) {
		if (node != null) {
			final IBinding binding = node.resolveBinding();
			/* Handle variable bindings like constants and enums e.g. in class instantiations: 'new Test(Enum1.A)' and assignments: 'String mig = Class1.CONST1' */
			if (binding instanceof IVariableBinding) {
				final ITypeBinding targetBinding = ((IVariableBinding) binding).getDeclaringClass();
				if (targetBinding != null) {
					String qualifiedName =
							targetBinding.isParameterizedType() ? targetBinding.getErasure().getQualifiedName() : targetBinding.getQualifiedName();
					qualifiedName = qualifiedName.isEmpty() ? targetBinding.getName() : qualifiedName;
					addDependencyChecked(node, qualifiedName, getPackageNameSafe(targetBinding), DependencyType.REFERENCE);
				}
			}
		}

		return true;
	}

	private void visitType(final Type type) {
		ASTNode parent = type.getParent();
		if (parent instanceof ParameterizedType) {
			parent = parent.getParent();
		}

		final ITypeBinding typeBinding = type.resolveBinding();
		if (typeBinding != null) {
			if (parent instanceof AbstractTypeDeclaration) {
				final ITypeBinding parentBinding = ((AbstractTypeDeclaration) parent).resolveBinding();

				for (ITypeBinding interfaceType : parentBinding.getInterfaces()) {
					if (typeBinding == interfaceType) {
						handleTypeBinding(type, interfaceType, parentBinding.isInterface() ? DependencyType.EXTEND : DependencyType.IMPLEMENT);
						return;
					}
				}

				final ITypeBinding baseclassType = parentBinding.getSuperclass();
				if (baseclassType == typeBinding) {
					handleTypeBinding(type, baseclassType, DependencyType.EXTEND);
					return;
				}
			}
			if ( ! typeBinding.isTypeVariable()) {
				handleTypeBinding(type, typeBinding, DependencyType.REFERENCE);
			}
		}
	}

	private void handleTypeBinding(final Type node, final ITypeBinding binding, final DependencyType type) {
		/* for parameterized types binding.getQualifiedName() returns PACKAGE.Interface9<PACKAGE.Interface7>*/
		String qualifiedName = binding.isParameterizedType() ? binding.getErasure().getQualifiedName() : binding.getQualifiedName();
		qualifiedName = qualifiedName.isEmpty() ? binding.getName() : qualifiedName;
		addDependencyChecked(node, qualifiedName, getPackageNameSafe(binding), type);
	}

	private static String getPackageNameSafe(final ITypeBinding binding) {
		return binding.getPackage() == null ? "" : binding.getPackage().getName();
	}

	private void addDependencyChecked(final ASTNode node, final String qualifiedName, final String packageName, final DependencyType type) {
		if (typeDeclarations.isEmpty()) {
			dependencies.add(new Dependency(node, qualifiedName, packageName, type, ""));
		} else if ( ! typeDeclarations.peek().equals(qualifiedName)) {
			dependencies.add(new Dependency(node, qualifiedName, packageName, type, typeDeclarations.peek()));
		}
	}

	/**
	 * A wrapper class for {@link Annotation}s and {@link Type}s in compilation units.
	 */
	static class Dependency extends AbstractNode<ASTNode> {

		/** The type of the reference */
		public enum DependencyType {
			/** The type (class or enum) implements the found type */
			IMPLEMENT,
			/** The type (class or interface) extends the found type */
			EXTEND,
			/** The found type is an annotation */
			ANNOTATION,
			/** All other: The type references the found type */
			REFERENCE
		}

		private final DependencyType type;
		private final String name;
		private final String parentName;
		private final String packageName;

		private Dependency(final ASTNode node, final String name, final String packageName, final DependencyType type, final String parentName) {
			super(node);
			this.type = type;
			this.name = name;
			this.parentName = parentName;
			this.packageName = packageName;
		}

		/**
		 * Returns the fully qualified type name.
		 * 
		 * @return fully qualified name; not {@code null}
		 */
		public String getFullyQualifiedName() {
			return name;
		}

		/**
		 * Returns the package name of the dependency or an empty string if the package name is not available..
		 * 
		 * @return package name; not {@code null}
		 */
		public String getPackageName() {
			return packageName;
		}

		/**
		 * Returns the fully qualified type name of the enclosing parent type.
		 * 
		 * @return fully qualified name; not {@code null}
		 */
		public String getParentName() {
			return parentName;
		}

		@Override
		public String toString() {
			return String.format("Dependency: %s, Type: %s, Parent: %s, Package: %s, %s", name, type.toString(), parentName, packageName, super.toString());
		}

		/**
		 * Returns the {@link DependencyType} of this {@link Dependency}.
		 *
		 * @return the {@link DependencyType}; not {@code null}
		 */
		public DependencyType getType() {
			return type;
		}
	}

	/**
	 * A wrapper class for {@link MethodInvocation} that return an instance of {@link CallableStatement}, used to execute Stored Procedures.
	 */
	static class StoredProcedureInvocation extends AbstractNode<MethodInvocation> {

		private final boolean isSqlPrepareCall;

		private StoredProcedureInvocation(final MethodInvocation node, final boolean isSqlPrepareCall) {
			super(node);
			this.isSqlPrepareCall = isSqlPrepareCall;
		}

		/**
		 * Returns the name of the Stored Procedure (SP) that gets called.<br><br>
		 * Currently the SP name is examined only if the method of the underlying {@link MethodInvocation} has only one argument. The argument has to be a
		 * String and must match with the SP call pattern: {@code "CALL (<SCHEMA><.>)?<SPNAME><(>"}.
		 * 
		 * @return The name of the stored procedure or {@code null}, if the name couldn't be examined.
		 */
		public Optional<String> getStoredProcedure() {
			if (node.arguments().size() == 1) {
				final Expression param = (Expression) node.arguments().iterator().next();
				if (param instanceof StringLiteral) {
					return Optional.ofNullable(getStoredProcedureNameFromCall(((StringLiteral) param).getLiteralValue()));
				}
			}

			return Optional.empty();
		}

		@Override
		public String toString() {
			return String.format("Stored Procedure Call: %s, %s",
					isSqlPrepareCall ? "is SQL connection.prepareCall()" : "invoking method that returns a java.sql.CallableStatement", super.toString());
		}
	}

	/**
	 * A wrapper class for {@link Annotation}s and {@link Type}s in compilation units.
	 */
	static class JavaType extends AbstractNode<ASTNode> {

		private final ModuleType type;
		private final String name;

		JavaType(final AbstractTypeDeclaration node, final ModuleType type) {
			super(node);
			this.type = type;
			final ITypeBinding typeBinding = assertNotNull(node.resolveBinding());
			this.name = typeBinding.getQualifiedName().isEmpty() ? typeBinding.getName() : typeBinding.getQualifiedName();
		}

		JavaType(final MethodDeclaration node, final ModuleType type, final String name) {
			super(node);
			this.type = type;
			this.name = name;
		}

		/**
		 * @return the fully qualified name
		 */
		public String getFullyQualifiedName() {
			return name;
		}

		/**
		 * @return the {@link ModuleType} of this type.
		 */
		public ModuleType getType() {
			return type;
		}
	
		@Override
		public String toString() {
			return String.format("TypeNode: %s, Type: %s, %s", name, type.toString(), super.toString());
		}
	}

	/**
	 * Base wrapper class for {@link ASTNode}s.
	 * @param <N> {@link ASTNode} type
	 */
	abstract static class AbstractNode<N extends ASTNode> {

		/** The {@link ASTNode} that is wrapped. */
		protected final N node;

		private AbstractNode(final N node) {
			this.node = node;
		}

		/**
		 * Returns a string representation of this statement suitable for information purposes only.
		 *
		 * @return The string representation; not {@code null}
		 */
		public String getCode() {
			return node.toString();
		}

		/**
		 * Returns the offset of the stored procedure method call.
		 *
		 * @return The 0-based index or {@code 0}, if source position information is unavailable
		 */
		public int getOffset() {
			return node.getStartPosition();
		}

		/**
		 * Returns the length of the stored procedure method call.
		 * 
		 * @return The length or {@code 0}, if source position information is unavailable
		 */
		public int getLength() {
			return node.getLength();
		}

		/**
		 * Returns the line of the stored procedure method call.
		 * 
		 * @return The line or {@code 0}, if source position information is unavailable
		 */
		public int getLine() {
			return ((CompilationUnit) node.getRoot()).getLineNumber(node.getStartPosition());
		}

		@Override
		public String toString() {
			return String.format("Offset: %d, Length: %d, Line: %d, Code: %s",
					Integer.valueOf(getOffset()), Integer.valueOf(getLength()), Integer.valueOf(getLine()), getCode());
		}
	}

	/**
	 * Model class to represent a method call in a Java class.
	 */
	public static class MethodCall extends AbstractNode<ASTNode> {

		private final String caller;
		private final String callerType;
		private final String callee;
		private final String calleeType;
		private final String[] parameters;
		private final String packageName;

		private MethodCall(final ASTNode node, final String caller, final String callerType, final String callee, final String calleeType,
				final String[] parameters, final String packageName) {
			super(node);

			this.caller = caller;
			this.callerType = callerType;
			this.callee = callee;
			this.calleeType = calleeType;
			this.parameters = parameters;
			this.packageName = packageName;
		}

		/**
		 * @return the fully qualified method name of the method caller
		 */
		public String getCallerMethod() {
			return callerType + "." + caller;
		}

		/**
		 * @return the fully qualified method name of the called method
		 */
		public String getCalleeMethod() {
			return calleeType + "." + callee;
		}

		/**
		 * @return the fully qualified names of the method parameters or an empty error if the called method has no parameters
		 */
		public String[] getCalleeMethodParameters() {
			return parameters;
		}

		/**
		 *
		 * @return the package name
		 */
		public String getPackageName() {
			return packageName;
		}

		@Override
		public String toString() {
			return String.format("MethodCall: Offset: %d, Length: %d, Caller: %s(), Called method: %s(), Method parameters: %s, Package: %s",
					Integer.valueOf(getOffset()), Integer.valueOf(getLength()), getCallerMethod(), getCalleeMethod(), Arrays.toString(parameters), packageName);
		}
	}

	/**
	 * @return the document
	 */
	public Document getDocument() {
		return this.document;
	}
}
