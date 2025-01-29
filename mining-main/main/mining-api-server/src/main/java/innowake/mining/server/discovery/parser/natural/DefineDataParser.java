/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.natural;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.util.List;
import java.util.Optional;
import org.apache.commons.collections4.CollectionUtils;
import innowake.lib.core.lang.NonNullByDefault;
import innowake.lib.parsing.util.visitor.TopDown;
import innowake.lib.parsing.util.visitor.VisitableCollection;
import innowake.lib.parsing.util.visitor.Visitor;
import innowake.mining.server.discovery.parser.natural.DataField.FieldType;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.naturalparser.NaturalSystemContext;
import innowake.ndt.naturalparser.ast.DataArea.AivDataDefinitions;
import innowake.ndt.naturalparser.ast.DataArea.GlobalDataDefinitions;
import innowake.ndt.naturalparser.ast.DataArea.LocalDataDefinitions;
import innowake.ndt.naturalparser.ast.DataArea.ParameterDataDefinitions;
import innowake.ndt.naturalparser.ast.DataDefinition.Parameter;
import innowake.ndt.naturalparser.ast.DataDefinition.Variable;
import innowake.ndt.naturalparser.ast.Expression;
import innowake.ndt.naturalparser.ast.Expression.IntegerConstant;
import innowake.ndt.naturalparser.ast.Expression.Ref;
import innowake.ndt.naturalparser.ast.Expression.StringConstant;
import innowake.ndt.naturalparser.ast.Expression.VariableArrayConstant;
import innowake.ndt.naturalparser.ast.FormatLength;
import innowake.ndt.naturalparser.ast.FormatLength.AlphaNumeric;
import innowake.ndt.naturalparser.ast.FormatLength.Binary;
import innowake.ndt.naturalparser.ast.FormatLength.FloatingPoint;
import innowake.ndt.naturalparser.ast.FormatLength.Numeric;
import innowake.ndt.naturalparser.ast.FormatLength.PackedNumeric;
import innowake.ndt.naturalparser.ast.FormatLength.UniCode;
import innowake.ndt.naturalparser.ast.Index.IndexRange;
import innowake.ndt.naturalparser.ast.Index.SimpleIndex;
import innowake.ndt.naturalparser.ast.InitValue.InitExpression;
import innowake.ndt.naturalparser.ast.Initialization;
import innowake.ndt.naturalparser.ast.Initialization.ConstantVariable;
import innowake.ndt.naturalparser.ast.Initialization.InitializedVariable;
import innowake.ndt.naturalparser.ast.NaturalNode;
import innowake.ndt.naturalparser.ast.ParameterKind;
import innowake.ndt.naturalparser.ast.ParameterKind.ByValue;
import innowake.ndt.naturalparser.ast.Reference;
import innowake.ndt.naturalparser.ast.Reference.Var;
import innowake.ndt.naturalparser.ast.Statement.DefineDataStmt;
import innowake.ndt.naturalparser.ast.VariableType;
import innowake.ndt.naturalparser.ast.VariableType.Array;
import innowake.ndt.naturalparser.util.NaturalUtils;


/**
 * Parser for define data blocks in Natural code.
 * <p>
 * Almost originally copied from {@code /ndt-analysis-scripts/expert-base/innowake/expert/base/ndt/parser/definedata/DefineDataParser.java}.
 * Should be adapted to code guidelines if someone finds time.
 */
@NonNullByDefault(false)
public class DefineDataParser {

	public static final int DYNAMIC_LENGTH = Integer.MAX_VALUE;
	public static final int DYNAMIC_CONST_LENGTH = Integer.MIN_VALUE;
	
	private final String decimalSeparator;
	private DataArea dataArea;
	private boolean inView;
	private final INaturalModel naturalModel;

	public DefineDataParser(final NaturalSystemContext sysContext, final INaturalModel naturalModel) {
		assertNotNull(sysContext);
		decimalSeparator = String.valueOf(sysContext.getDecimalSeparator());
		this.naturalModel = naturalModel;
	}
	
	public Optional<DataArea> parse() throws DiscoveryException {
		final DefineDataStmt ddStmt = getDDStatement(naturalModel);
		dataArea = null;
		if (ddStmt != null) {
			dataArea = new DataArea();
			new TopDown(new Visitor() {

				private FieldType fieldType = null;

				@Override
				public boolean visit(Object node) {
					if (node instanceof GlobalDataDefinitions) {
						fieldType = FieldType.GLOBAL;
						return true;
					} else if (node instanceof ParameterDataDefinitions) {
						fieldType = FieldType.PARAMETER;
						return true;
					} else if (node instanceof LocalDataDefinitions) {
						fieldType = FieldType.LOCAL;
						return true;
					} else if (node instanceof AivDataDefinitions) {
						fieldType = FieldType.INDEPENDENT;
						return true;
					} else {
						final DataField df = parse(node);
						if (df != null) {
							df.setType(fieldType);
							dataArea.addField(df);
							return false;
						}
					}
					return true;
				}
			}).visit(ddStmt);
			dataArea.finalizeStructure();
		}
		return Optional.ofNullable(dataArea);
	}

	private Group parse(final innowake.ndt.naturalparser.ast.DataDefinition.Group natGroup) {
		final int level = natGroup.getLevel();
		final Group group = createGroup(natGroup);
		group.setLevel(level);
		final VisitableCollection indizes = natGroup.getIndizes();
		final int[] arrayDef = getArrayDefinition(indizes, dataArea);
		group.setRanges(arrayDef);
		final int[] occs = getOccurrences(arrayDef);
		group.setOccurrences(occs);
		final VisitableCollection definitions = natGroup.getDefinitions();
		for (int i = 0; i < definitions.size(); i++) {
			DataField df = parse(definitions.getChildAt(i));
			if (df != null) {
				if (inView) {
					df = groupToField(df);
				}
				group.addField(df);
			}
		}
		return group;
	}

	private View parse(final innowake.ndt.naturalparser.ast.DataDefinition.View natView) {
		final View view = createView(natView);
		final int level = natView.getLevel();
		view.setLevel(level);
		view.setDDMName(natView.getDdmName().toString());
		final VisitableCollection definitions = natView.getDefinitions();
		inView = true;
		for (int i = 0; i < definitions.size(); i++) {
			DataField df = parse(definitions.getChildAt(i));
			if (df != null) {
				df = groupToField(df);
				view.addField(df);
			}
		}
		inView = false;
		return view;
	}
	
	private DataField groupToField(final DataField df) {
		DataField result = df;
		if (df.isGroup()) {
			/* check if it as real group or just a view field without format/length definition */
			final Group group = (Group) df;
			if (group.getFields().isEmpty()) {
				/* no children => no real group */
				result = createDataField(group.getNaturalNode(), group.getFieldName());
				result.setLevel(group.getLevel());
				result.setType(group.getType());
				result.setOccurrences(group.getOccurrences());
				result.setRanges(group.getRanges());
			}
		}
		return result;
	}
	
	private Redefinition parse(final innowake.ndt.naturalparser.ast.DataDefinition.Redefinition redefine) {
		final Redefinition def = createRedefinition(redefine);
		def.setLevel(redefine.getLevel());
		final VisitableCollection definitions = redefine.getDefinitions();
		for (int i = 0; i < definitions.size(); i++) {
			final Object child = definitions.getChildAt(i);
			final DataField df = parse(child);
			if (df != null) {
				def.addField(df);
			}
		}
		return def;
	}
	
	private DataField parse(final Parameter param) {
		final DataField df = createDataField(param, NaturalUtils.getQualfiedName(param.getQualName()));
		final int level = param.getLevel();
		df.setLevel(level);
		final VariableType type = param.getType();
		if (type.isArray()) {
			final Array array = (Array) type;
			final int[] arrayDef = getArrayDefinition(array.getIndizes(), dataArea);
			df.setRanges(arrayDef);
			final int[] occurrences = getOccurrences(arrayDef);
			/* set occs for field level */
			df.setOccurrences(occurrences); 
		}
		final ParameterKind paramKind = param.getKind();
		final boolean byValue = paramKind.isByValue();
		if (byValue) {
			df.setByValue(byValue);
			final ByValue bvKind = (ByValue) paramKind;
			df.setByValueResult(bvKind.getResult());
		}
		df.setOptional(param.getOptional());
		final FormatLength format = type.getFormat();
		df.setNaturalType(getNaturalFormatType(format));
		df.setLength(getFieldLength(format));
		df.setsLength(getFieldLengthAsString(format, decimalSeparator));
		return df;
	}
	
	private DataField parse(final Variable var) {
		final DataField df = createDataField(var);
		int level = var.getLevel();
		df.setLevel(level);
		final VariableType type = var.getType();
		if (type.isArray()) {
			final Array array = (Array) type;
			final int[] arrayDef = getArrayDefinition(array.getIndizes(), dataArea);
			df.setRanges(arrayDef);
			final int[] occurrences = getOccurrences(arrayDef);
			/* set occs for field level */
			df.setOccurrences(occurrences); 
		}
		final FormatLength format = type.getFormat();
		df.setNaturalType(getNaturalFormatType(format));
		df.setLength(getFieldLength(format));
		df.setsLength(getFieldLengthAsString(format, decimalSeparator));
		final Initialization initType = var.getInit();
		final boolean constantVariable = initType.isConstantVariable();
		if (constantVariable || initType.isConstantArray()) {
			df.setConstant(true);
			if (constantVariable) {
				df.setConstValue(getConstValue((ConstantVariable) initType));
			}
		}
		if (initType.isInitializedVariable()) {
			final InitializedVariable initVar = (InitializedVariable) initType;
			df.setInitialValue(getInitialValue(initVar));
		}
		return df;
	}

	private DataField parse(final Object o) {
		DataField df = null;
		if (o instanceof innowake.ndt.naturalparser.ast.imp.ImpDataDefinition.Group) {
			df = parse((innowake.ndt.naturalparser.ast.imp.ImpDataDefinition.Group) o);
		} else if (o instanceof innowake.ndt.naturalparser.ast.imp.ImpDataDefinition.Redefinition) {
			df = parse((innowake.ndt.naturalparser.ast.imp.ImpDataDefinition.Redefinition) o);
		} else if (o instanceof innowake.ndt.naturalparser.ast.imp.ImpDataDefinition.View) {
			df = parse((innowake.ndt.naturalparser.ast.imp.ImpDataDefinition.View) o);
		} else if (o instanceof Parameter) {
			df = parse((Parameter) o);
		} else if (o instanceof Variable) {
			df = parse((Variable) o);
		}
		return df;
	}
	
	private DefineDataStmt getDDStatement(final INaturalModel natModel) throws DiscoveryException {
		DefineDataStmt stmt = null;
		if (natModel.parsingPossible()) {
			final VisitableCollection statements = natModel.getProgramObject().getStatements();
			for (int i = 0; i < statements.size() && stmt == null; i++) {
				if (statements.getChildAt(i) instanceof DefineDataStmt) {
					stmt = (DefineDataStmt) statements.getChildAt(i);
				}
			}
		} else {
			throw new DiscoveryException("parsing of given model not possible!");
		}
		return stmt;
	}
	
	private String getConstValue(final ConstantVariable constVar) {
		if (constVar.getInitValue().isInitExpression()) {
			return getConstValue((InitExpression) constVar.getInitValue());
		}
		return null;
	}
	
	private String getInitialValue(final InitializedVariable initVar) {
		if (initVar.getInitValue().isInitExpression()) {
			return getConstValue((InitExpression) initVar.getInitValue());
		}
		return null;
	}
	
	/* get constant value (only integer and string constants supported at the moment) */
	private String getConstValue(final InitExpression init) {
		final Expression value = init.getValue();
		if (value.isIntegerConstant()) {
			return ((IntegerConstant) value).getBigInteger().toString();
		} else if (value.isStringConstant()) {
			return ((StringConstant) value).getStringValue().toString();
		}
		return null;
	}
	
	private int[] getOccurrences(final int[] arrayDef) {
		final int[] occurrences = new int[] { -1, -1 , -1};
		for (int i = 0, j = 0; i < 6; i+=2, j++) {
			if (arrayDef[i] != -1) {
				int occs = arrayDef[i + 1];
				if (arrayDef[i] > 1) {
					occs = arrayDef[ i + 1] - arrayDef[i] + 1; 
				}
				occurrences[j] = occs;
			}
		}
		return occurrences;
	}

	/*
	 * Returns an array with size=6. For every possible dimension there are two 
	 * entries (lower and upper bounds). -1 if no array is defined for
	 * specific dimension. <code>DYNAMIC_LENGTH</code> is used for dynamic occurrences and
	 * <code>DYNAMIC_CONST_LENGTH</code> is used if the dimension uses a const ("V") for dynamic 
	 * occurrences. If a pre-defined var is used as upper bound for a dimension the used var is searched
	 * within the given data area. If the const-var was found its value is used as occurrence.
	 *
	 * <b>example:</b>
	 * <ul>
	 * <li>(1:2,1:3) results in [1,2,1,3,-1,-1]</li>
	 * <li>(1:2,2:10,*) results in [1,2,2,10,1,<code>DYNAMIC_LENGTH</code>]</li>
	 * <li>(1:2,2:10,1:V) results in [1,2,2,10,1,<code>DYNAMIC_CONST_LENGTH</code>]</li>
	 * <li>(1:2,2:10,1:#UPPER) results in [1,2,2,10,1,12</li> if #UPPER is defined as <code>CONST<12></code>
	 * </ul>
	 * @param arrayDef, the visitable collection containing the index-definitions
	 * @return an array with size=6. For every possible dimension there are two 
	 * entries (lower and upper bounds)
	 */
	private int[] getArrayDefinition(final VisitableCollection arrayDef, final DataArea area) {
		final int[] ranges = new int[] { -1, -1, -1, -1, -1, -1 };
		
		final int dim = arrayDef.size();
		
		if (dim > 0) {
			for (int i = 0; i <= dim * 2 - 2; i+=2) {
				/* initialize array dims */
				ranges[i + 0] = 1;
				ranges[i + 1] = DYNAMIC_LENGTH;
			}
					
			new TopDown(new Visitor() {
	
				int actDim = 0;
				
				@Override
				public boolean visit(Object o) {
					if (o instanceof IndexRange) {
						final IndexRange irange = (IndexRange) o;
						if (irange.getFrom() instanceof IntegerConstant) {
							final IntegerConstant intConst = (IntegerConstant) irange.getFrom();
							ranges[actDim + 0] = Integer.parseInt(intConst.getBigInteger().toString());
						}
						if (irange.getTo() instanceof IntegerConstant) {
							final IntegerConstant intConst = (IntegerConstant) irange.getTo();
							ranges[actDim + 1] = Integer.parseInt(intConst.getBigInteger().toString());
						} else if (irange.getTo() instanceof VariableArrayConstant) {
							ranges[actDim + 1] = DYNAMIC_CONST_LENGTH;
						} else if (irange.getTo() instanceof Ref) {
							Ref ref = (Ref) irange.getTo();
							final Reference target = ref.getTarget();
							if (target instanceof Var) {
								/* check data area for numeric const */
								if (area != null) {
									ranges[actDim + 1] = getConstBound(area, target);
								}
								/* var and no matching constant found */
								if (ranges[ actDim + 1] == 0) {
									ranges[actDim + 1] = DYNAMIC_CONST_LENGTH;
								}
							}
						}
						actDim+=2;
						return false;
					} else if (o instanceof SimpleIndex) {
						final SimpleIndex index = (SimpleIndex) o;
						if (index.getIdx() instanceof IntegerConstant) {
							final IntegerConstant intConst = (IntegerConstant) index.getIdx();
							ranges[actDim + 1] = Integer.parseInt(intConst.getBigInteger().toString());
						} else if (index.getIdx() instanceof VariableArrayConstant) {
							ranges[actDim + 1] = DYNAMIC_CONST_LENGTH;
						} else if (index.getIdx() instanceof Ref) {
							final Ref ref = (Ref) index.getIdx();
							final Reference target = ref.getTarget();
							if (target instanceof Var) {
								/* check data area for numeric const */
								if (area != null) {
									ranges[actDim + 1] = getConstBound(area, target);
								}	
								/* var and no matching constant found */
								if (ranges[ actDim + 1] == 0) {
									ranges[actDim + 1] = DYNAMIC_CONST_LENGTH;
								}
							}
						}
						actDim+=2;
						return false;
					}
					return true;
				}

				private int getConstBound(final DataArea area, final Reference target) {
					final Var var = (Var) target;
					final String varName = NaturalUtils.getQualfiedName(var.getQualName());
					final List<DataField> fields = area.resolve(varName);
					if (CollectionUtils.isNotEmpty(fields)) {
						for (DataField constField : fields) {
							if (constField.isConstant()) {
								final Optional<String> naturalType = constField.getNaturalType();
								if (naturalType.isPresent() &&
										(naturalType.get().equals("I")
										|| naturalType.get().equals("P")
										|| naturalType.get().equals("N"))) {
									try {
										return Integer.parseInt(constField.getConstValue().orElse(null));
									} catch (final NumberFormatException e) {
										return DYNAMIC_CONST_LENGTH;
									}
								}
								break;
							}
						}
					}
					return DYNAMIC_CONST_LENGTH;
				}
				
			}).visit(arrayDef);
		}
		return ranges;
	}
	
	/*
	 * @param formatLength the formatLength of the field
	 * @return the natural string representation of the format type
	 */
	private String getNaturalFormatType(final FormatLength formatLength) {
		if (formatLength == null) {
			return "0";
		}
		if (formatLength.isAlphaNumeric() || formatLength.isAlphaNumericDynamic()) {
			return "A";
		} else if (formatLength.isBinary() || formatLength.isBinaryDynamic()) {
			return "B";
		} else if (formatLength.isDate()) {
			return "D";
		} else if (formatLength.isFloatingPoint()) {
			return "F";
		} else if (formatLength.isInteger()) {
			return "I";
		} else if (formatLength.isLogical()) {
			return "L";
		} else if (formatLength.isNumeric()) {
			return "N";
		} else if (formatLength.isPackedNumeric()) {
			return "P";
		} else if (formatLength.isTime()) {
			return "T";
		} else if (formatLength.isAttributeControl()) {
			return "C";
		} else if (formatLength.isHandle()) {
			return "HANDLE";
		} else if (formatLength.isUniCode() || formatLength.isUniCodeDynamic()) {
			return "U";
	    } else {
			throw new IllegalStateException("Unhandled format " + formatLength);
		}
	}
	
	/* 
	 * Calculates field length for given formatLength
	 * 
	 * @param formatLength the formatLength of the field
	 * @return the calculated length or <code>DYNAMIC_LENGTH</code> if 
	 * the field has dynamic length
	 */
	private int getFieldLength(final FormatLength formatLength) {
		int result = -1;
		if(formatLength == null) {
			return 0;
		}
		if (formatLength.isAlphaNumeric()) {
			final AlphaNumeric a = (AlphaNumeric) formatLength;
			result = a.getLength();
		} else if (formatLength.isAlphaNumericDynamic()) {
			result = DYNAMIC_LENGTH;
		}
		else if (formatLength.isBinary()) {
			Binary b = (Binary) formatLength;
			result = b.getLength();
		} else if (formatLength.isBinaryDynamic()) {
			result = DYNAMIC_LENGTH;
		} else if (formatLength.isDate()) {
			result = 4;
		} else if (formatLength.isFloatingPoint()) {
			FloatingPoint fp = (FloatingPoint) formatLength;
			result = fp.getLength();
		} else if (formatLength.isInteger()) {
			final innowake.ndt.naturalparser.ast.FormatLength.Integer i = 
				(innowake.ndt.naturalparser.ast.FormatLength.Integer) formatLength;
			result = i.getLength();
		} else if (formatLength.isLogical()) {
			result = 1;
		} else if (formatLength.isNumeric()) {
			final Numeric n = (Numeric) formatLength;
			result = n.getPostDecimal() + n.getPreDecimal();
		} else if (formatLength.isPackedNumeric()) {
			final PackedNumeric pn = (PackedNumeric) formatLength;
			result = pn.getPostDecimal() + pn.getPreDecimal();
			result = (result + 2)/2;
		} else if (formatLength.isTime()) {
			result = 7;
		} else if (formatLength.isUniCode()) {
			final UniCode uc = (UniCode) formatLength;
			result = uc.getLength();
		} else if (formatLength.isUniCodeDynamic()) {
			result = DYNAMIC_LENGTH;
		} else if (formatLength.isAttributeControl()) {
			return 2;
		} else if (formatLength.isHandle()) {
			return 0;
	    } else {
			throw new IllegalStateException("Unhandled format " + formatLength);
		}
		return result;
	}
	
	/*
	 * Returns the string representation of the field length using
	 * the given decimal separator. If a field has no alphanumeric-
	 * length (L, C, D, T) an empty String ("") is returned.
	 * 
	 * @param formatLength formatLength of the field
	 * @param decimalSeparator the separator string for numeric fields
	 * @return string representation of the field length
	 */
	private String getFieldLengthAsString(final FormatLength formatLength, final String decimalSeparator) {
		final String result;
		if (formatLength == null) {
			result = "";
		} else if (formatLength.isAlphaNumeric()) {
			final AlphaNumeric a = (AlphaNumeric) formatLength;
			result = Integer.toString(a.getLength());
		} else if (formatLength.isAlphaNumericDynamic()) {
			result = "*";
		} else if (formatLength.isBinary()) {
			final Binary b = (Binary) formatLength;
			result = Integer.toString(b.getLength());
		} else if (formatLength.isBinaryDynamic()) {
			result = "*";
		} else if (formatLength.isDate()) {
			result = "";
		} else if (formatLength.isFloatingPoint()) {
			final FloatingPoint fp = (FloatingPoint) formatLength;
			result = Integer.toString(fp.getLength());
		} else if (formatLength.isInteger()) {
			final innowake.ndt.naturalparser.ast.FormatLength.Integer i = 
				(innowake.ndt.naturalparser.ast.FormatLength.Integer) formatLength;
			result = Integer.toString(i.getLength());
		} else if (formatLength.isLogical()) {
			result = "";
		} else if (formatLength.isNumeric()) {
			final Numeric n = (Numeric) formatLength;
			result = Integer.valueOf(n.getPreDecimal()) + decimalSeparator 
				+ Integer.valueOf(n.getPostDecimal());
		} else if (formatLength.isPackedNumeric()) {
			final PackedNumeric pn = (PackedNumeric) formatLength;
			result = Integer.valueOf(pn.getPreDecimal()) + decimalSeparator + Integer.valueOf(pn.getPostDecimal());
		} else if (formatLength.isTime()) {
			result = "";
		} else if (formatLength.isUniCode()) {
			final UniCode uc = (UniCode) formatLength;
			result = Integer.toString(uc.getLength());
		} else if (formatLength.isUniCodeDynamic()) {
			result = "*";
		} else if (formatLength.isAttributeControl()) {
			result = "";
		} else if (formatLength.isHandle()) {
			result = "";
	    } else {
			throw new IllegalStateException("Unhandled format " + formatLength);
		}
		return result;
	}

	private View createView(final innowake.ndt.naturalparser.ast.DataDefinition.View view) {
		return new View(view.getName().toString(), FieldType.LOCAL, view);
	}

	private Redefinition createRedefinition(final innowake.ndt.naturalparser.ast.DataDefinition.Redefinition redefinition) {
		return new Redefinition("", FieldType.LOCAL, redefinition, redefinition.getName().toString());
	}

	private DataField createDataField(final NaturalNode node, final String name) {
		return new DataField(name, FieldType.LOCAL, node);
	}

	
	private DataField createDataField(final Variable variable, final FieldType type) {
		return new DataField(variable.getName().toString(), type, variable);
	}

	private DataField createDataField(final Variable variable) {
		return createDataField(variable, FieldType.LOCAL);
	}
	
	private Group createGroup(final innowake.ndt.naturalparser.ast.DataDefinition.Group group) {
		return new Group(group.getName().toString(), FieldType.LOCAL, group);
	}
	
}
