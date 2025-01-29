import { Command } from 'commander';

export interface CommandProvider {
    getCommand(): Command;
}