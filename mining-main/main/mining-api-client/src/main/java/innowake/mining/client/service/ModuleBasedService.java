/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import com.fasterxml.jackson.core.type.TypeReference;
import innowake.lib.common.job.Job;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Base class for all REST services that are based on modules.
 *
 * @param <T> the type of the actual service
 */
public abstract class ModuleBasedService<T extends ProjectIdService<T, String>> extends ProjectIdService<T, String>{

    List<String> modulePaths = new ArrayList<>();

    List<EntityId> moduleIds = new ArrayList<>();

    /**
     * Creates a new instance.
     *
     * @param connectionInfo the connection info to use
     */
    public ModuleBasedService(final ConnectionInfo connectionInfo) {
        super(connectionInfo);
    }

    @Override
    protected void validate() {
        super.validate();
        if (modulePaths.isEmpty() && moduleIds.isEmpty()) {
            throw new IllegalStateException("At least one module id or path must be specified");
        }
    }


    /**
     * Sets the paths of all modules to identify the candidates for.
     *
     * @param modulePaths the paths of all modules
     * @return {@code this}
     */
    public T setModulePaths(final List<String> modulePaths) {
        this.modulePaths = modulePaths;
        return getThis();
    }

    /**
     * Sets the ids of all modules to identify the candidates for.
     *
     * @param moduleIds the paths of all modules
     * @return {@code this}
     */
    public T setModuleIds(final List<EntityId> moduleIds) {
        this.moduleIds = moduleIds;
        return getThis();
    }

    /**
     * Call execution of the specific job for the given modules by sending a HTTP GET request to the specified {@code  endPoint},
     * which will start a {@link Job}.
     * <p>
     * Use the {@link JobServiceProvider} to check the status of the job.
     * <p>
     * Returns the following status codes:
     * <li><strong>202</strong>: on successful job submission
     * <li><strong>404</strong>: if the given project does not exist
     *
     * @return a result holding the ID of the {@link Job} if the call was successful
     */
    @Override
    public Result<String> execute() throws IOException {
        validate();
        setServiceUrl(String.format(getEndPoint(), encode(projectId)));
        final HttpPost post = httpPost();
        final ModuleMatcher moduleMatcher = new ModuleMatcher(moduleIds, modulePaths);
        post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(moduleMatcher), ContentType.APPLICATION_JSON));
        return execute(post, new TypeReference<String>() {});
    }

    public abstract String getEndPoint();
}
