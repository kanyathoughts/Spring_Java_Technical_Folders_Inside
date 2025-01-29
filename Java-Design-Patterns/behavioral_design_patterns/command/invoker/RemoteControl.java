package command.invoker;

import command.command_classes.Command;

public class RemoteControl {
    Command command;

    public void pressButton() {
        command.execute();
    }

    public void setCommand(Command command) {
        this.command = command;
    }

}
