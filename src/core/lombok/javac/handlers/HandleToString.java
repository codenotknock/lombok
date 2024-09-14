/*
 * Copyright (C) 2009-2021 The Project Lombok Authors.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package lombok.javac.handlers;

import static lombok.core.handlers.HandlerUtil.*;
import static lombok.javac.handlers.JavacHandlerUtil.*;
import static lombok.javac.Javac.*;

import java.util.Collection;

import lombok.ConfigurationKeys;
import lombok.ToString;
import lombok.core.AnnotationValues;
import lombok.core.configuration.CallSuperType;
import lombok.core.configuration.CheckerFrameworkVersion;
import lombok.core.AST.Kind;
import lombok.core.handlers.InclusionExclusionUtils;
import lombok.core.handlers.InclusionExclusionUtils.Included;
import lombok.javac.JavacAnnotationHandler;
import lombok.javac.JavacNode;
import lombok.javac.JavacTreeMaker;
import lombok.spi.Provides;

import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.tree.JCTree.JCAnnotation;
import com.sun.tools.javac.tree.JCTree.JCArrayTypeTree;
import com.sun.tools.javac.tree.JCTree.JCBlock;
import com.sun.tools.javac.tree.JCTree.JCExpression;
import com.sun.tools.javac.tree.JCTree.JCMethodDecl;
import com.sun.tools.javac.tree.JCTree.JCMethodInvocation;
import com.sun.tools.javac.tree.JCTree.JCModifiers;
import com.sun.tools.javac.tree.JCTree.JCPrimitiveTypeTree;
import com.sun.tools.javac.tree.JCTree.JCStatement;
import com.sun.tools.javac.tree.JCTree.JCTypeParameter;
import com.sun.tools.javac.tree.JCTree.JCVariableDecl;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.ListBuffer;

/**
 * Handles the {@code ToString} annotation for javac.
 */
@Provides
public class HandleToString extends JavacAnnotationHandler<ToString> {
	@Override public void handle(AnnotationValues<ToString> annotation, JCAnnotation ast, JavacNode annotationNode) {
		handleFlagUsage(annotationNode, ConfigurationKeys.TO_STRING_FLAG_USAGE, "@ToString");
		
		deleteAnnotationIfNeccessary(annotationNode, ToString.class);
		
		ToString ann = annotation.getInstance();
		boolean onlyExplicitlyIncluded = annotationNode.getAst().getBooleanAnnotationValue(annotation, "onlyExplicitlyIncluded", ConfigurationKeys.TO_STRING_ONLY_EXPLICITLY_INCLUDED);
		java.util.List<Included<JavacNode, ToString.Include>> members = InclusionExclusionUtils.handleToStringMarking(annotationNode.up(), onlyExplicitlyIncluded, annotation, annotationNode);
		if (members == null) return;
		
		Boolean callSuper = ann.callSuper();
		
		if (!annotation.isExplicit("callSuper")) callSuper = null;
		
		Boolean doNotUseGettersConfiguration = annotationNode.getAst().readConfiguration(ConfigurationKeys.TO_STRING_DO_NOT_USE_GETTERS);
		boolean doNotUseGetters = annotation.isExplicit("doNotUseGetters") || doNotUseGettersConfiguration == null ? ann.doNotUseGetters() : doNotUseGettersConfiguration;
		FieldAccess fieldAccess = doNotUseGetters ? FieldAccess.PREFER_FIELD : FieldAccess.GETTER;
		
		boolean includeFieldNames = annotationNode.getAst().getBooleanAnnotationValue(annotation, "includeFieldNames", ConfigurationKeys.TO_STRING_INCLUDE_FIELD_NAMES);
		// filterNull 默认值写死了，false 默认使用lombok原生的toString
//		boolean filterNull = annotationNode.getAst().getBooleanAnnotationValue(anno, "filterNull", false);
		generateToString(annotationNode.up(), annotationNode, members, includeFieldNames, callSuper, true, fieldAccess, true);
	}
	
	public void generateToStringForType(JavacNode typeNode, JavacNode errorNode) {
		if (hasAnnotation(ToString.class, typeNode)) {
			//The annotation will make it happen, so we can skip it.
			return;
		}
		
		AnnotationValues<ToString> anno = AnnotationValues.of(ToString.class);
		boolean includeFieldNames = typeNode.getAst().getBooleanAnnotationValue(anno, "includeFieldNames", ConfigurationKeys.TO_STRING_INCLUDE_FIELD_NAMES);
		boolean onlyExplicitlyIncluded = typeNode.getAst().getBooleanAnnotationValue(anno, "onlyExplicitlyIncluded", ConfigurationKeys.TO_STRING_ONLY_EXPLICITLY_INCLUDED);
		// filterNull 默认值写死了，false 默认使用lombok原生的toString
//		boolean filterNull = typeNode.getAst().getBooleanAnnotationValue(anno, "filterNull", false);


		Boolean doNotUseGettersConfiguration = typeNode.getAst().readConfiguration(ConfigurationKeys.TO_STRING_DO_NOT_USE_GETTERS);
		FieldAccess access = doNotUseGettersConfiguration == null || !doNotUseGettersConfiguration ? FieldAccess.GETTER : FieldAccess.PREFER_FIELD;
		
		java.util.List<Included<JavacNode, ToString.Include>> members = InclusionExclusionUtils.handleToStringMarking(typeNode, onlyExplicitlyIncluded, null, null);
		generateToString(typeNode, errorNode, members, includeFieldNames, null, false, access, true);
	}
	
	public void generateToString(JavacNode typeNode, JavacNode source, java.util.List<Included<JavacNode, ToString.Include>> members,
		boolean includeFieldNames, Boolean callSuper, boolean whineIfExists, FieldAccess fieldAccess, boolean filterNull) {

		if (!isClassOrEnum(typeNode)) {
			source.addError("@ToString is only supported on a class or enum.");
			return;
		}
		
		switch (methodExists("toString", typeNode, 0)) {
		case NOT_EXISTS:
			if (callSuper == null) {
				if (isDirectDescendantOfObject(typeNode)) {
					callSuper = false;
				} else {
					CallSuperType cst = typeNode.getAst().readConfiguration(ConfigurationKeys.TO_STRING_CALL_SUPER);
					if (cst == null) cst = CallSuperType.SKIP;
					switch (cst) {
					default:
					case SKIP:
						callSuper = false;
						break;
					case WARN:
						source.addWarning("Generating toString implementation but without a call to superclass, even though this class does not extend java.lang.Object. If this is intentional, add '@ToString(callSuper=false)' to your type.");
						callSuper = false;
						break;
					case CALL:
						callSuper = true;
						break;
					}
				}
			}
			JCMethodDecl method;
			if (!typeNode.isEnumType() && filterNull) {
				method = createToStringV2(typeNode, members, includeFieldNames, callSuper, fieldAccess, source);
			} else {
				method = createToString(typeNode, members, includeFieldNames, callSuper, fieldAccess, source);
			}

			injectMethod(typeNode, method);
			break;
		case EXISTS_BY_LOMBOK:
			break;
		default:
		case EXISTS_BY_USER:
			if (whineIfExists) {
				source.addWarning("Not generating toString(): A method with that name already exists");
			}
			break;
		}
	}
	
	static JCMethodDecl createToString(JavacNode typeNode, Collection<Included<JavacNode, ToString.Include>> members,
		boolean includeNames, boolean callSuper, FieldAccess fieldAccess, JavacNode source) {
		
		JavacTreeMaker maker = typeNode.getTreeMaker();

		// 添加 Override 注解 和 public 修饰符; 根据 Checker Framework 的设置（如果需要），可能添加 @SideEffectFree 注解
		JCAnnotation overrideAnnotation = maker.Annotation(genJavaLangTypeRef(typeNode, "Override"), List.<JCExpression>nil());
		List<JCAnnotation> annsOnMethod = List.of(overrideAnnotation);
		if (getCheckerFrameworkVersion(typeNode).generateSideEffectFree()) annsOnMethod = annsOnMethod.prepend(maker.Annotation(genTypeRef(typeNode, CheckerFrameworkVersion.NAME__SIDE_EFFECT_FREE), List.<JCExpression>nil()));
		JCModifiers mods = maker.Modifiers(Flags.PUBLIC, annsOnMethod);
		JCExpression returnType = genJavaLangTypeRef(typeNode, "String");

		boolean first = true;
		
		String typeName = getTypeName(typeNode);
		boolean isEnum = typeNode.isEnumType();

		String infix = ", ";
		String suffix = ")";
		String prefix;
		if (callSuper) {
			prefix = "(super=";
		} else if (members.isEmpty()) {
			prefix = isEnum ? "" : "()";
		} else if (includeNames) {
			Included<JavacNode, ToString.Include> firstMember = members.iterator().next();
			String name = firstMember.getInc() == null ? "" : firstMember.getInc().name();
			if (name.isEmpty()) name = firstMember.getNode().getName();
			prefix = "(" + name + "=";
		} else {
			prefix = "(";
		}
		
		JCExpression current;
		if (!isEnum) {
			// 创建一个表示字符串 typeName 加上 prefix 的字面量表达式: "NotifyTrade1("
			current = maker.Literal(typeName + prefix);
		} else {
			current = maker.Binary(CTC_PLUS, maker.Literal(typeName + "."), maker.Apply(List.<JCExpression>nil(),
					maker.Select(maker.Ident(typeNode.toName("this")), typeNode.toName("name")),
					List.<JCExpression>nil()));
			if (!prefix.isEmpty()) current = maker.Binary(CTC_PLUS, current, maker.Literal(prefix));
		}
		
		
		if (callSuper) {
			JCMethodInvocation callToSuper = maker.Apply(List.<JCExpression>nil(),
				maker.Select(maker.Ident(typeNode.toName("super")), typeNode.toName("toString")),
				List.<JCExpression>nil());
			// 拼接父类的 toString方法: "NotifyTrade1(" + super.tostring()
			current = maker.Binary(CTC_PLUS, current, callToSuper);
			first = false;
		}
		
		for (Included<JavacNode, ToString.Include> member : members) {
			JCExpression expr;
			
			JCExpression memberAccessor;
			JavacNode memberNode = member.getNode();
			if (memberNode.getKind() == Kind.METHOD) {
				// 生成对方法的调用表达式
				memberAccessor = createMethodAccessor(maker, memberNode);
			} else {
				// 生成对属性的访问表达式
				memberAccessor = createFieldAccessor(maker, memberNode, fieldAccess);
			}

			// 获取字段的类型信息并移除注解: 编译后的代码无须注解
			JCExpression memberType = removeTypeUseAnnotations(getFieldType(memberNode, fieldAccess));
			
			// The distinction between primitive and object will be useful if we ever add a 'hideNulls' option.
			@SuppressWarnings("unused")
			// 检查 memberType 是否为基本类型（JCPrimitiveTypeTree）
			boolean fieldIsPrimitive = memberType instanceof JCPrimitiveTypeTree;
			// 检查 memberType 是否为基本类型数组（JCArrayTypeTree），且其元素类型为基本类型（JCPrimitiveTypeTree）
			boolean fieldIsPrimitiveArray = memberType instanceof JCArrayTypeTree && ((JCArrayTypeTree) memberType).elemtype instanceof JCPrimitiveTypeTree;
			// 检查 memberType 是否为对象数组（JCArrayTypeTree），且不是基本类型数组
			boolean fieldIsObjectArray = !fieldIsPrimitiveArray && memberType instanceof JCArrayTypeTree;

			// 生成表达式: 数组则单独toString方法，否则直接使用表达式
			if (fieldIsPrimitiveArray || fieldIsObjectArray) {
				JCExpression tsMethod = chainDots(typeNode, "java", "util", "Arrays", fieldIsObjectArray ? "deepToString" : "toString");
				expr = maker.Apply(List.<JCExpression>nil(), tsMethod, List.<JCExpression>of(memberAccessor));
			} else expr = memberAccessor;
			
			if (first) {
				current = maker.Binary(CTC_PLUS, current, expr);
				first = false;
				continue;
			}
			
			if (includeNames) {
				String n = member.getInc() == null ? "" : member.getInc().name();
				if (n.isEmpty()) n = memberNode.getName();
				// 创建一个包含分隔符、名称和等号的字面量 "NotifyTrade1(" + super.toString() + ", store_id="
				current = maker.Binary(CTC_PLUS, current, maker.Literal(infix + n + "="));
			} else {
				// 只添加分割符
				current = maker.Binary(CTC_PLUS, current, maker.Literal(infix));
			}

			// 将 expr 连接到 current 表达式, expr是表达式或者toString方法
			current = maker.Binary(CTC_PLUS, current, expr);
		}

		// 结尾的 ")" 拼接到 current
		if (!first) current = maker.Binary(CTC_PLUS, current, maker.Literal(suffix));

		// 使用 maker.Return(current) 创建一个返回语句，返回 current 表达式的值
		JCStatement returnStatement = maker.Return(current);

		// 创建一个代码块（方法体），包含 returnStatement 作为唯一的语句
		JCBlock body = maker.Block(0, List.of(returnStatement));

		// 创建一个方法定义，包括修饰符、方法名、返回类型、参数类型列表、参数列表、方法体、默认值
		JCMethodDecl methodDef = maker.MethodDef(mods, typeNode.toName("toString"), returnType,
			List.<JCTypeParameter>nil(), List.<JCVariableDecl>nil(), List.<JCExpression>nil(), body, null);
		createRelevantNonNullAnnotation(typeNode, methodDef);
		return recursiveSetGeneratedBy(methodDef, source);
	}

	/**
	 * 自定义 toString 方法， 生成的字符串会将字段值为null的值进行过滤
	 * 前面需要添加判断： 不是枚举类型 且 fieldNull = true  逻辑中直接省去了对枚举类型的判断
	 * @param typeNode
	 * @param members
	 * @param includeNames
	 * @param callSuper
	 * @param fieldAccess
	 * @param source
	 * @return
	 */
	static JCMethodDecl createToStringV2(JavacNode typeNode, Collection<Included<JavacNode, ToString.Include>> members,
										 boolean includeNames, boolean callSuper, FieldAccess fieldAccess, JavacNode source) {
		/**
		 *  原生: return "NotifyTrade1(super=" + super.toString() + ", store_id=" + this.getStore_id() + ")";
		 *  新的: 过滤了字段值为 null 属性
		 *  新的: return "{super.toString(),store_id=this.getStore_id(),}"
		 */
		JavacTreeMaker maker = typeNode.getTreeMaker();

		// 添加 Override 注解 和 public 修饰符; 根据 Checker Framework 的设置（如果需要），可能添加 @SideEffectFree 注解
		JCAnnotation overrideAnnotation = maker.Annotation(genJavaLangTypeRef(typeNode, "Override"), List.<JCExpression>nil());
		List<JCAnnotation> annsOnMethod = List.of(overrideAnnotation);
		if (getCheckerFrameworkVersion(typeNode).generateSideEffectFree())
			annsOnMethod = annsOnMethod.prepend(maker.Annotation(genTypeRef(typeNode, CheckerFrameworkVersion.NAME__SIDE_EFFECT_FREE), List.<JCExpression>nil()));
		JCModifiers mods = maker.Modifiers(Flags.PUBLIC, annsOnMethod);
		JCExpression returnType = genJavaLangTypeRef(typeNode, "String");

		String typeName = getTypeName(typeNode);
		boolean isEnum = typeNode.isEnumType();

		// 创建 StringBuilder 实例
		JCExpression stringBuilderType = genJavaLangTypeRef(typeNode, "StringBuilder");
		JCExpression stringBuilderConstructor = maker.NewClass(
				null,
				List.<JCExpression>nil(),
				stringBuilderType,
				List.<JCExpression>nil(),
				null
		);
		JCVariableDecl sbDecl = maker.VarDef(maker.Modifiers(0), typeNode.toName("sb"), stringBuilderType, stringBuilderConstructor);
		source.addWarning("生成 StringBuilder 实例.");

		// 获取sb 变量和 append 方法
		JCExpression sbIdent = maker.Ident(typeNode.toName("sb"));
		JCExpression appendMethod = maker.Select(sbIdent, typeNode.toName("append"));

		// 收集所有的语句
		ListBuffer<JCStatement> jcStatements = new ListBuffer<JCStatement>();
		jcStatements.append(sbDecl);


		String infix = ", ";
		String suffix = "}";
		String prefix;

		// 前缀
		if (members.isEmpty()) {
			prefix = "{}";
		}/* else if (includeNames) {
			Included<JavacNode, ToString.Include> firstMember = members.iterator().next();
			String name = firstMember.getInc() == null ? "" : firstMember.getInc().name();
			if (name.isEmpty()) name = firstMember.getNode().getName();
			prefix = "{" + name + "=";
		}*/ else {
			prefix = "{";
		}
		// sb.append("{");
		jcStatements.append(
				maker.Exec(
						maker.Apply(List.<JCExpression>nil(),
								appendMethod,
								List.<JCExpression>of((maker.Literal("{")))
						))
		);
		source.addWarning("生成 字符串前缀 实例.");

		// sb.append(super.tostring());
		if (callSuper) {
			JCExpression superIdent = maker.Ident(typeNode.toName("super"));
			JCExpression toStringMethod = maker.Select(superIdent, typeNode.toName("toString"));
			jcStatements.append(
					maker.Exec(
							maker.Apply(List.<JCExpression>nil(),
									appendMethod,
									List.of(toStringMethod)
							))
			);
		}


		for (Included<JavacNode, ToString.Include> member : members) {
			JCExpression expr;

			JCExpression memberAccessor;
			JavacNode memberNode = member.getNode();
			if (memberNode.getKind() == Kind.METHOD) {
				// 生成对方法的调用表达式
				memberAccessor = createMethodAccessor(maker, memberNode);
			} else {
				// 生成对属性的访问表达式
				memberAccessor = createFieldAccessor(maker, memberNode, fieldAccess);
			}

			// 获取字段的类型信息并移除注解: 编译后的代码无须注解
			JCExpression memberType = removeTypeUseAnnotations(getFieldType(memberNode, fieldAccess));

			// The distinction between primitive and object will be useful if we ever add a 'hideNulls' option.
			@SuppressWarnings("unused")
			// 检查 memberType 是否为基本类型（JCPrimitiveTypeTree）
			boolean fieldIsPrimitive = memberType instanceof JCPrimitiveTypeTree;
			// 检查 memberType 是否为基本类型数组（JCArrayTypeTree），且其元素类型为基本类型（JCPrimitiveTypeTree）
			boolean fieldIsPrimitiveArray = memberType instanceof JCArrayTypeTree && ((JCArrayTypeTree) memberType).elemtype instanceof JCPrimitiveTypeTree;
			// 检查 memberType 是否为对象数组（JCArrayTypeTree），且不是基本类型数组
			boolean fieldIsObjectArray = !fieldIsPrimitiveArray && memberType instanceof JCArrayTypeTree;

			// 生成表达式: 数组则单独toString方法，否则直接使用表达式
			if (fieldIsPrimitiveArray || fieldIsObjectArray) {
				JCExpression tsMethod = chainDots(typeNode, "java", "util", "Arrays", fieldIsObjectArray ? "deepToString" : "toString");
				expr = maker.Apply(List.<JCExpression>nil(), tsMethod, List.<JCExpression>of(memberAccessor));
			} else expr = memberAccessor;

			if (!fieldIsPrimitive) {
				// 添加字段值为 null 的判断 if(getField != null) sb.append(getField)
	//			JCExpression nullCheck = maker.Binary(CTC_EQUAL, expr, maker.Literal(CTC_BOT, null));
	//			JCExpression isNull = maker.Binary(CTC_EQUAL, maker.Select(maker.Ident(thisName), name), maker.Literal(CTC_BOT, null));
	//			JCExpression valueOrNull = maker.Conditional(nullCheck, maker.Literal(""), expr);


//				JCExpression cond = maker.Binary(CTC_NOT_EQUAL, thisDotField, maker.Literal(CTC_BOT, null));

				JCExpression fieldNotNullCondition = maker.Binary(CTC_NOT_EQUAL, expr, maker.Literal(CTC_BOT, null));
				JCStatement appendStatement = maker.Exec(
						maker.Apply(List.<JCExpression>nil(),
								appendMethod,
								List.<JCExpression>of(maker.Literal(memberNode.getName() + "="))
						)
				);
				JCStatement appendStatement1 =maker.Exec(
						maker.Apply(List.<JCExpression>nil(),
								appendMethod,
								List.of(expr)
						)
				);

				JCStatement appendStatement2 = maker.Exec(
						maker.Apply(List.<JCExpression>nil(),
								appendMethod,
								List.<JCExpression>of(maker.Literal(infix))
						)
				);



	//			source.addError("判断字段。。。.");
	//			JCStatement clearKeyCall = maker.Exec(maker.Apply(jceBlank, thisDotKeyFieldDotClear, jceBlank));
	//			JCStatement clearValueCall = maker.Exec(maker.Apply(jceBlank, thisDotValueFieldDotClear, jceBlank));
	//			JCExpression cond = maker.Binary(CTC_NOT_EQUAL, thisDotKeyField, maker.Literal(CTC_BOT, null));
	//			JCBlock clearCalls = maker.Block(0, List.of(clearKeyCall, clearValueCall));
	//			JCStatement ifSetCallClear = maker.If(cond, clearCalls, null);
				jcStatements.append(
						maker.If(fieldNotNullCondition, maker.Block(0, List.of(appendStatement, appendStatement1, appendStatement2)), null)
				);
			} else {
				jcStatements.append(
						maker.Exec(
								maker.Apply(List.<JCExpression>nil(),
										appendMethod,
										List.<JCExpression>of(maker.Literal(memberNode.getName() + "=" + expr + infix))
						)
				));
			}
		}
		// sb.append("{");
		jcStatements.append(
				maker.Exec(
						maker.Apply(List.<JCExpression>nil(),
								appendMethod,
								List.<JCExpression>of(maker.Literal(suffix)))
				)
		);

		// 使用 maker.Return(current) 创建一个返回语句，返回 sb.toString()
		jcStatements.append(
				maker.Return(
						maker.Apply(List.<JCExpression>nil(),
								maker.Select(maker.Ident(typeNode.toName("sb")), typeNode.toName("toString")),
								List.<JCExpression>nil()))
		);

		// 创建一个代码块（方法体），包含 returnStatement 作为唯一的语句
		JCBlock body = maker.Block(0, jcStatements.toList());

		// 创建一个方法定义，包括修饰符、方法名、返回类型、参数类型列表、参数列表、方法体、默认值
		JCMethodDecl methodDef = maker.MethodDef(mods, typeNode.toName("toString"), returnType,
				List.<JCTypeParameter>nil(), List.<JCVariableDecl>nil(), List.<JCExpression>nil(), body, null);
		createRelevantNonNullAnnotation(typeNode, methodDef);
		return recursiveSetGeneratedBy(methodDef, source);
	}
	
	public static String getTypeName(JavacNode typeNode) {
		String typeName = typeNode.getName();
		JavacNode upType = typeNode.up();
		while (upType.getKind() == Kind.TYPE && !upType.getName().isEmpty()) {
			typeName = upType.getName() + "." + typeName;
			upType = upType.up();
		}
		return typeName;
	}
}
