/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.error;

/**
 * This exception covers an error when an entity cannot be read, created, updated or deleted due to constraint violation.
 */
public class ConstraintViolationException extends RuntimeException {

    /**
     * Creates an instance.
     * 
     * @param entity the entity which cannot be created
     * @param reason the reason for the constraint violation
     * @param cause the original exception
     */
    public <T> ConstraintViolationException(final T entity, final String reason, final Throwable cause) {
        super("Constraint violation for: " + entity.toString() + ". Reason: " + reason, cause);
    }
    
    /**
     * Creates an instance.
     * 
     * @param entity the entity which cannot be created
     * @param reason the reason for the constraint violation
     */
    public <T> ConstraintViolationException(final T entity, final String reason) {
        super("Constraint violation for: " + entity.toString() + ". Reason: " + reason);
    }
    
    /**
     * Creates an instance.
     * 
     * @param reason the reason for the constraint violation
     * @param cause the original exception
     */
    public <T> ConstraintViolationException(final String reason, final Throwable cause) {
        super("Constraint violation. Reason: " + reason, cause);
    }
    
    /**
     * Creates an instance.
     * 
     * @param reason the reason for the constraint violation
     */
    public <T> ConstraintViolationException(final String reason) {
    	super("Constraint violation. Reason: " + reason);
    }

}