TypeSafeMatcher<Double> match = new TypeSafeMatcher<Double>() {

    @Override
    public void describeTo(Description arg0) {
    }

    @Override
    public boolean matchesSafely(Double arg0) {

        System.out.println( arg0 );

        //test that something is <=100
        return !( arg0 > 100 );
    }
};

Assert.assertThat( "Oh no it's > 100!", 100.34, match );
