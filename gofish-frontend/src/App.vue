<template>
<h1>Hello world!</h1>
<router-view />
</template>

<script>
import Home from "./components/Home.vue";
export default {
    name: "App",
    components: {
        Home
    },
    data(){
        return{

        }
    }
}
</script>

<style>

</style>
