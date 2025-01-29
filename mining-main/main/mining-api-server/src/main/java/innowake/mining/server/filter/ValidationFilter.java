package innowake.mining.server.filter;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

/**
 * Filter checking the existence of the {@link Authentication} and a non-empty name of the principal.
 */
@Component
public class ValidationFilter implements Filter {

	@Override
	public void doFilter(final ServletRequest request, final ServletResponse response, final FilterChain chain) throws IOException, ServletException {
		final Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
    	if (authentication == null || authentication.getName().isEmpty()) {
    		((HttpServletResponse) response).sendError(HttpStatus.UNAUTHORIZED.value());
    		return;
    	}

		chain.doFilter(request, response);
	}

}
