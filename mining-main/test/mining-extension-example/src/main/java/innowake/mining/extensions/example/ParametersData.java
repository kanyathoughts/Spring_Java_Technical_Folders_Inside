package innowake.mining.extensions.example;

import com.google.common.collect.Streams;
import innowake.mining.shared.io.ParameterDescription;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class ParametersData {

    private ParametersData() {
        /* private constructor to hide implicit one */
    }

    public static List<ParameterDescription> getStringParametersData() {
        /* ParameterDescription with mandatory string value and single value */
        final ParameterDescription stringDescription1 = new ParameterDescription("stringName", "test value for string type",
                ParameterDescription.ParameterType.STRING, true, StringUtils.EMPTY, false, "test value example",
                Collections.emptyList());
        /* ParameterDescription with optional  string value and single value */
        final ParameterDescription stringDescription2 = new ParameterDescription("stringNameOptional", "optional test value for string type",
                ParameterDescription.ParameterType.STRING, false, "OptionalDefaultValue", false, "Optional1",
                Collections.emptyList());
        /* ParameterDescription with mandatory string value and multiple value */
        final ParameterDescription stringDescription3 = new ParameterDescription("stringNameMultipleMandatory",
                "Mandatory multiple test value for string type",
                ParameterDescription.ParameterType.STRING, true, StringUtils.EMPTY, true, "TestString",
                Collections.emptyList());
        /* ParameterDescription with optional  string value and multiple value */
        final ParameterDescription stringDescription4 = new ParameterDescription("stringNameMultipleOptional", "optional multiple test value for string type",
                ParameterDescription.ParameterType.STRING, false, "OptionalDefaultValueMultiple", true, "Optional1",
                Collections.emptyList());
        /* ParameterDescription with mandatory  string value and allowable values */
        final ParameterDescription stringDescription5 = new ParameterDescription("MandatoryAllowableString", "mandatory allowable test value for string type",
                ParameterDescription.ParameterType.STRING, true, StringUtils.EMPTY, false, "Mandatory1",
                Arrays.asList("Mandatory1", "Mandatory2", "Mandatory3"));
        /* ParameterDescription with optional  string value and allowable values */
        final ParameterDescription stringDescription6 = new ParameterDescription("OptionalAllowableString", " optional allowable test value for string type",
                ParameterDescription.ParameterType.STRING, false, "Mandatory3", false, "Mandatory1",
                Arrays.asList("Mandatory1", "Mandatory2", "Mandatory3"));

        return Arrays.asList(stringDescription1, stringDescription2, stringDescription3, stringDescription4, stringDescription5, stringDescription6);
    }

    public  static  List<ParameterDescription> getNumberParametersData() {

        /* ParameterDescription with mandatory string value and single value */
        final ParameterDescription numberDescription1 = new ParameterDescription("NumberTest", "test value for number type",
                ParameterDescription.ParameterType.NUMBER, true, StringUtils.EMPTY, false, "2",
                Collections.emptyList());
        /* ParameterDescription with optional  string value and single value */
        final ParameterDescription numberDescription2 = new ParameterDescription("NumberTestOptional", "optional test value for number type",
                ParameterDescription.ParameterType.NUMBER, false, "1", false, "1",
                Collections.emptyList());
        /* ParameterDescription with mandatory  number value and allowable values */
        final ParameterDescription numberDescription3 = new ParameterDescription("NumberWithAllowableValues", "allowable test value for number type",
                ParameterDescription.ParameterType.NUMBER, true, StringUtils.EMPTY, false, "1",
                Arrays.asList("1", "2", "3"));

        /* ParameterDescription with optional number value and allowable values */
        final ParameterDescription numberDescription4 = new ParameterDescription("NumberWithOptionalAllowableValues", "allowable test value for number type",
                ParameterDescription.ParameterType.NUMBER, false, "1", true, "Mandatory1",
                Arrays.asList("1", "2", "3"));

        return Arrays.asList(numberDescription1, numberDescription2, numberDescription3, numberDescription4);
    }

    public  static  List<ParameterDescription> getBooleanParametersData() {

        /* ParameterDescription with mandatory Boolean value  */
        final ParameterDescription booleanDescription1 = new ParameterDescription("BooleanTest", "test value for Boolean type",
                ParameterDescription.ParameterType.BOOLEAN, true, "false", false, "True",
                Collections.emptyList());
        /* ParameterDescription with optional  Boolean value */
        final ParameterDescription booleanDescription2 = new ParameterDescription("BooleanTestOptional", "optional test value for Boolean type",
                ParameterDescription.ParameterType.BOOLEAN, false, "true", false, "True",
                Collections.emptyList());

        return Arrays.asList(booleanDescription1, booleanDescription2);
    }

    public static List<ParameterDescription> getAllParameterData() {
       return  Streams.concat(getStringParametersData().stream(), getNumberParametersData().stream(), getBooleanParametersData().stream())
                .collect(Collectors.toList());
    }
}
