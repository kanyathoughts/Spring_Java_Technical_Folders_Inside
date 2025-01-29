import { Component, NgZone, OnDestroy, OnInit } from '@angular/core';
import { ClientProjectRelationshipService } from '@app/core/services/client-project-relationship/client-project-relationship.service';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { LoaderState } from '@app/shared/loader/loader.directive';
import { ClientProjectRelationship } from '@app/shared/models/client-project-relationship.model';
import { Subscription } from 'rxjs';
import { FunctionalBlock } from '../functional-block.interface';
import { ActivatedRoute } from '@angular/router';
import { Title } from '@angular/platform-browser';
import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'app-functional-blocks-layout',
  templateUrl: './functional-blocks-layout.component.html',
  styleUrls: ['./functional-blocks-layout.component.less']
})
export class FunctionalBlocksLayoutComponent implements OnInit, OnDestroy {
  loadState: LoaderState;
  clientId: number;
  projectId: number;
  fcBlock: FunctionalBlock[] = [];
  finalFcBlock: { [key: string]: any[] } = {};
  projectName: string;
  level: number;

  private clientProjectSubscription: Subscription;
  private currentClient: ClientProjectRelationship;

  constructor(
    private graphQlControllerService: GraphQlControllerService,
    private clientProjectRelationShip: ClientProjectRelationshipService,
    private route: ActivatedRoute,
    private titleService: Title,
    private translateService: TranslateService,
    private zone: NgZone
  ) {
    this.clientProjectSubscription = this.clientProjectRelationShip.getClientProjectObservable().subscribe(currentClient => {
      this.currentClient = currentClient;
      this.clientId = currentClient.getClientId();
      this.projectId = currentClient.getProjectId();
      this.projectName = currentClient.getProjectName();
    });
  }

  ngOnInit(): void {
    let uuid: string;
    this.route.params.subscribe(params => {
      uuid = params['uuid'];
      const selectedCard = {
        child: uuid
      };
      this.fetchFunctionalBlockByLevel(0, selectedCard);
    });
  }

  /**
   * Method to fetch functional block data based on uid.
   * @param event user clicked event containing child data.
   * @param index index of the functional block user clicked.
   */
  openChildCard(event: { parent: FunctionalBlock, child: string }, index: number): void {
    let isBlockExists = [];
    this.fcBlock.forEach((block: FunctionalBlock) => {
      isBlockExists = block?.childBlock?.filter((c: any) => c.uid === event.child);
    });
    if ( ! isBlockExists?.length) {
      this.fetchFunctionalBlockByLevel(index + 1, event);
      this.level = index + 1;
    }
  }

  ngOnDestroy(): void {
    this.clientProjectSubscription?.unsubscribe();
  }

  /**
   * method to remove the particular card
   * @param uid: card id
   */
  closeCard(uid: string): void {
    const filteredChildBlock: FunctionalBlock[] = [];
    this.fcBlock.forEach((block) => {
      const childArray: string[] = [];
      if(block.uid === uid && block.childBlock.length){
        block.childBlock.forEach((parent: FunctionalBlock) => {
          childArray.push(parent.uid);
        });
        childArray.forEach((childUid: string) => {
          this.closeCard(childUid);
        });
      }
      if (block.uid !== uid) {
        if (block.childBlock) {
          block.childBlock = block.childBlock.filter (
            (child: any) => child.uid !== uid);
        }
        filteredChildBlock.push(block);
      }
    });
    this.fcBlock = filteredChildBlock;
  }

  private fetchFunctionalBlockByLevel(level: number, selectedCard: { parent?: FunctionalBlock, child: string }): void {
    const query = this.generateGraphQLQuery(level, selectedCard?.child);
    this.graphQlControllerService.graphQl({
      query,
    }).subscribe((response: any) => {
      if (response && response?.data?.functionalBlock) {
        if (selectedCard?.parent) {
          this.buildChildBlockRecursively(response.data.functionalBlock as FunctionalBlock, selectedCard);
        } else {
          if (level === 0) {
            this.titleService.setTitle(this.translateService.instant(response.data.functionalBlock.name
              + '- Functional Block - ' + this.projectName) as string);
          }
          this.fcBlock.push(response.data.functionalBlock as FunctionalBlock);
        }
      }
    });
  }

  private buildChildBlockRecursively(functionalBlock: FunctionalBlock, selectedCard: any): void {
    // checking if parent already exists and get its index
    const indexToInsert = this.fcBlock?.findIndex((fc: any) => fc.uid === selectedCard.parent.uid);
    // creating paramater childBlock at the selected parent index and pushing the contents
    if (indexToInsert !== -1) {
      // inserting child blocks in the order they are in parent block
      if (selectedCard?.parent) {
        const childLevel: number = selectedCard.parent?.children.content.findIndex((x: any) => x.uid === functionalBlock.uid);
        if (this.fcBlock[indexToInsert]['childBlock']) {
          this.fcBlock[indexToInsert]['level'] = this.level === 1 ? 0:  this.level -1;
          this.fcBlock[indexToInsert]['childBlock'].splice(childLevel, 0, functionalBlock);
        } else {
          this.fcBlock = this.fcBlock.map(fc => ({ ...fc, childBlock: [functionalBlock] }));
        }
      }
    } else {
      this.fcBlock.push({ ...selectedCard.parent, childBlock: [] } as FunctionalBlock);
      this.buildChildBlockRecursively(functionalBlock, selectedCard);
    }
    this.zone.run(() => {
    this.finalFcBlock = this.convertToLevelBasedArray(this.fcBlock);
    });
  }

private convertToLevelBasedArray(data: FunctionalBlock[]) {
    const levelBasedArray = {};
    data.forEach(obj => {
        const level = obj.level ? obj.level: 0;
        if ( ! levelBasedArray[level]) {
            levelBasedArray[level] = [];
        }
        levelBasedArray[level].push(obj);
    });
    return levelBasedArray;
}

  private generateGraphQLQuery(level: number, uid: string): string {
    const fields = this.generateFields(level);
    return `{functionalBlock(projectId: "${this.projectId}", uid: "${uid}") {${fields}}}`;
  }

  private generateFields(level: number): string {
    if (level < 0) {
      return 'name uid description';
    } else {
      const nestedFields = this.generateFields(level - 1);
      return `name uid description children {content {${nestedFields}}}`;
    }
  }
}
