export default {
    console: console,
    error: function(msg: string) {
        console.error("Error: " + msg);
    },
    createArray: function() {
        return ([]);
    }
} as any;
