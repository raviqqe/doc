export default {
  plugins: [
    {
      name: "preset-default",
      params: {
        overrides: {
          // biome-ignore lint/style/useNamingConvention: External API
          removeXMLProcInst: false,
        },
      },
    },
  ],
};
