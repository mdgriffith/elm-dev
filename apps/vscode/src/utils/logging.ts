import * as vscode from 'vscode';

// Shared output channel for all Elm Dev logging
export const outputChannel = vscode.window.createOutputChannel('Elm Dev');

export function log(message: string) {
  outputChannel.appendLine(message);
}

export function logAndShow(message: string) {
  outputChannel.appendLine(message);
  outputChannel.show();
}