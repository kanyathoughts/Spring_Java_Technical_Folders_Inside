/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.api;

import innowake.mining.shared.entities.ModulePojo;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingDataProvider2;
import innowake.ndt.jcl.parser.assembling.IContentProvider;


/**
 * Data provider for different parsers.
 */
public interface DataProvider extends IAssemblingDataProvider<ModulePojo>, IAssemblingDataProvider2<ModulePojo>, IContentProvider {

}
