package innowake.mining.data.datapoints.registry;

import java.util.*;

import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.annotation.Order;

import innowake.mining.data.datapoints.MiningDataPointSource;

/**
 * Comparator for the Spring @Order Annotation.
 * Used for Sorting the DataPointSources, lower number has precedence over higher, e.g. 1 comes before 2
 */
public class SpringOrderComparingComparator implements Comparator<MiningDataPointSource> {

    @Override
	public int compare(MiningDataPointSource dps1, MiningDataPointSource dps2) {
        final Order annotation1 = AnnotationUtils.findAnnotation(dps1.getClass(), Order.class);
        final Order annotation2 = AnnotationUtils.findAnnotation(dps2.getClass(), Order.class);

        if (annotation1 == annotation2) {
            return 0;
        }
        if (annotation1 == null) {
            return 1;
        }
        if (annotation2 == null) {
            return -1;
        }
        double value1 = annotation1.value();
        double value2 = annotation2.value();
        //highest precedence is the lowest number, else 1 and 2 would be swapped
        if (value1 < value2) {
            return -1;
        }
        if (value1 > value2) {
            return 1;
        }
        return 0;
    }
}