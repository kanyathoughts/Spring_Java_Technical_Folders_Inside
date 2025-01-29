package command.command_classes;

import command.receiver.Light;

public class LightOffCommand implements Command {

    boolean lightOn = true;

    Light light;

    public LightOffCommand(Light light) {
        this.light = light;
    }

    @Override
    public void execute() {
        if (lightOn) {
            light.off();
            lightOn = false;
        } else {
            System.out.println("Already turned off");
        }

    }

}
