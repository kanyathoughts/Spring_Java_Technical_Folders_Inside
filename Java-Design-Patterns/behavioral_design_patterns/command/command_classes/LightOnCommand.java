package command.command_classes;

import command.receiver.Light;

public class LightOnCommand implements Command {

    // Obviously Light off will be true as light is off.
    // After turning the light on then you can set this value as false then when you
    // are trying to turn the light on then it will give warning that light is
    // already turned on
    boolean lightOff = true;

    Light light;

    public LightOnCommand(Light light) {
        this.light = light;
    }

    @Override
    public void execute() {
        if (lightOff) {
            light.on();
            lightOff = false;
        } else {
            System.out.println("Already turned on");
        }

    }

}
