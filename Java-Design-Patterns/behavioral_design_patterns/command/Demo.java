package command;

import command.command_classes.Command;
import command.command_classes.LightOffCommand;
import command.command_classes.LightOnCommand;
import command.invoker.RemoteControl;
import command.receiver.Light;

public class Demo {

    public static void main(String[] args) {
        Command lightOnCommand = new LightOnCommand(new Light());
        Command lightOffCommand = new LightOffCommand(new Light());

        RemoteControl rc = new RemoteControl();

        // Turn the light on
        rc.setCommand(lightOnCommand);
        rc.pressButton();

        rc.pressButton();

        // Turn the light off
        rc.setCommand(lightOffCommand);
        rc.pressButton();

        rc.pressButton();

    }

}
