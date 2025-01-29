/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.java.type;

import java.util.function.Consumer;

import innowake.mining.server.discovery.metrics.java.type.test.Interface1;
import innowake.mining.server.discovery.metrics.java.type.test.Interface5;

public interface TestInterface2 extends Interface1 {

    default <X> boolean method3(X xxx) {
        return xxx != null;
    }

    default <Y extends Interface5> boolean method3(Y yyy) {
    	return yyy != null;
    }

    default <Z> boolean method4(final Z zzz) {
    	final Consumer<Z> consumer = myVar -> {
    		final Z tmp = zzz;
    		if (tmp != null) {
        		throw new RuntimeException(zzz.toString());
        	}
    	};
    	consumer.accept(zzz);

        return true;
    }

}