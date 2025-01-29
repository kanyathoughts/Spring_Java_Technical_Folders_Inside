import { TaxonomyFilterSelected } from '@app/shared/components/taxonomy-filter/taxonomy-filter-selected.interface';
import { TaxonomyDetails, TaxonomyList, TaxonomyType } from '@app/shared/interfaces/taxonomy-list.interface';
import { TaxonomyPojo } from '@innowake/mining-api-angular-client';

/**
 * Method to group the taxonomies by Taxonomy Category and then by Taxonomy Type.
 * @param newList contains the list of taxonomy types.
 * @param currentValue value of taxonomy type in the list.
 * @returns Taxonomy Object containing taxonomies by Taxonomy Type.
 */
export const taxonomyReduceList = (newList: TaxonomyPojo, currentValue: TaxonomyPojo): TaxonomyPojo => {
    newList[currentValue.type.category.name] = newList[currentValue.type.category.name] || {};
    newList[currentValue.type.category.name][currentValue.type.name] =
    [...(newList[currentValue.type.category.name][currentValue.type.name] || []), {name: currentValue.name, id: currentValue.id}];
    return newList;
};

/**
 * Method to group the taxonomies into the treeNode.
 * @param taxonomyData contains the list of taxonomy.
 * @returns Taxonomy list in tree format.
 */
export const getTaxomomyTreeNode = (taxonomyData: TaxonomyPojo[]): TaxonomyList[] => {
    const groupedTaxonomy = taxonomyData.reduce((newList: TaxonomyPojo, currentValue: TaxonomyPojo) => taxonomyReduceList(newList, currentValue), {});
    const taxonomyList: TaxonomyList[] = [];
    Object.keys(groupedTaxonomy).forEach((category: string) => {
      const taxonomy_category: TaxonomyList = {
        name: category,
        type: [],
      };
      Object.keys(groupedTaxonomy[category] as object).forEach((type: string) => {
        const taxonomy_type: TaxonomyType = {
          name: type,
          taxonomies: [],
        };
        groupedTaxonomy[category][type].forEach((taxonomy: any) => {
          taxonomy_type.taxonomies.push({
            name: taxonomy?.name,
            id: taxonomy.id
          });
        });
        taxonomy_category.type.push(taxonomy_type);
      });
      taxonomyList.push(taxonomy_category);
      taxonomyList.sort((a, b) => a?.name.toLowerCase().localeCompare(b?.name.toLowerCase()));
      taxonomyList.forEach((type) => {
        type.type.sort((a, b) => a?.name.toLowerCase().localeCompare(b?.name.toLowerCase()));
        type.type.forEach((taxonomy) => {
          taxonomy.taxonomies.sort((a: { name: string, id: number }, b: { name: string, id: number }) =>
           a.name.toLowerCase().localeCompare(b.name.toLowerCase()));
        });
      });
    });

    return taxonomyList;
  };

/**
 * method to get taxonomy ids and taxonomy title
 * @param   filterDetail selected taxonomy
 * @returns return the object of title and taxonomy ids
 */
export const getTaxonomyIdsTitle = (filterDetail: TaxonomyFilterSelected[]): TaxonomyDetails  => {
  let taxonomyIds: number[] = [];
  const taxonomyTitles: string[] = [];
  const groupTaxonomy = {};
  filterDetail.forEach((filterDetailItem: TaxonomyFilterSelected) => {
    if (filterDetailItem.taxonomyTitle && filterDetailItem.taxonomyId !== 0) {
      taxonomyIds = [...taxonomyIds, ...`${filterDetailItem.taxonomyId}`.split(',').map(Number)];
      const [taxonomy, taxonomyChild] = filterDetailItem.taxonomyTitle.split(':');
      if ( ! groupTaxonomy[taxonomy]) {
        groupTaxonomy[taxonomy] = taxonomyChild ? [taxonomyChild] : [];
      } else {
        groupTaxonomy[taxonomy] = [...groupTaxonomy[taxonomy], taxonomyChild].sort();
      }
    }
  });
  Object.keys(groupTaxonomy).sort().forEach((taxonomyTitle: string) => {
    if (groupTaxonomy[taxonomyTitle].length) {
      taxonomyTitles.push(` ${taxonomyTitle}: ${groupTaxonomy[taxonomyTitle]}`);
    } else {
      taxonomyTitles.length ? taxonomyTitles.push(` ${taxonomyTitle}`): taxonomyTitles.push(taxonomyTitle);
    }
  });
  return {taxonomyIds, taxonomyTitles};
};
