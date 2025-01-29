/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.aspect;

import java.util.stream.StreamSupport;

import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.mining.server.util.UserNameUtil;
import innowake.mining.shared.model.UserIdentifyable;

/**
 * Aspect to populate UserName from the User id using the {@code UserNameUtil}.
 */
@Aspect
@Component
public class UserNameAspect {

	@Autowired
	private UserNameUtil userNameUtil;

	/**
	 * Matches the pointcut expression:
	 * <ul>
	 * 	<li>Method must have {@code Operation} annotated
	 * 	<li>Class must implement {@code BaseController}
	 * 	<li>Method return value must be an implementation of {@code Iterable} or {@code UserIdentifyable}
	 * </ul>
	 */
	@Pointcut("@annotation(io.swagger.v3.oas.annotations.Operation) && "
			+ "(execution(java.lang.Iterable+ innowake.mining.server.controller.BaseController+.*(..)) || "
			+ "execution(innowake.mining.shared.model.UserIdentifyable+ innowake.mining.server.controller.BaseController+.*(..)))")
	public void operationReturningUserIdentifyableInBaseController() {
		/* this method is just a named pointcut hence method is empty. */
	}

	/**
	 * The advice to call {@code UserNameUtil} to fill the Username from user id.
	 * 
	 * @param returnValue could be either {@code Iterable} or {@code UserIdentifyable}
	 */
	@AfterReturning(pointcut = "operationReturningUserIdentifyableInBaseController()", returning = "returnValue")
	public void resolveUserNameAdvice(final Object returnValue) {
		if (returnValue instanceof Iterable) {
			userNameUtil.fillUserNames(() -> StreamSupport.stream(((Iterable<?>) returnValue).spliterator(), false) /* do not make an attempt to parallel 
																														process this stream */
					.filter(UserIdentifyable.class::isInstance)
					.map(UserIdentifyable.class::cast)
					.iterator());
		} else if (returnValue instanceof UserIdentifyable) {
			userNameUtil.fillUserName((UserIdentifyable) returnValue);
		}
	}
}
