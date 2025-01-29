import { Directive, ElementRef, HostListener } from '@angular/core';
@Directive({
    selector: '[mn-table-ellipsis-row]'
})
export class TableRowEllipsisDirective {

    constructor(private el: ElementRef) { }

    @HostListener('mouseover') setPointerCursor(): void {
        const applyPointer = Array.from(this.el?.nativeElement?.querySelectorAll('td') as NodeListOf<HTMLElement>).some((childNode: any) =>
            (childNode.clientWidth < childNode.scrollWidth || childNode.clientHeight < childNode.scrollHeight)
        );
        if (applyPointer) {
            this.el.nativeElement.style['cursor'] = 'pointer';
        }
    }

    @HostListener('click', ['$event']) onRowClick(): void {
        const ellipsisElements = this.el.nativeElement.querySelectorAll('[nzEllipsis]');
        ellipsisElements.forEach((cell: HTMLElement) => {
            const cellStyle = 'white-space';
            if (cell.closest('td').attributes['nzEllipsis']) {
                const origValue = cell.closest('td').style[cellStyle];
                if (origValue !== 'pre-line') {
                    Object.values(cell.closest('td').parentNode.children).forEach((childNode: any) => {
                        childNode.style[cellStyle] = 'pre-line';
                    });
                }
            }
        });
    }
}
