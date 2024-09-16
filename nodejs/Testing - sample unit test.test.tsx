import { UseFormReturn } from 'react-hook-form';
import { CLIENT_APPLICATION_LIST } from 'oneidentity-entities/code-lists';
import ForgotPasswordPage from './page';
import { TolgeeInstance } from '@tolgee/react';
import { api } from '../../../../../trpc/server';

const mockForgotPasswordForm = { formState: { isSubmitSuccessful: false } } as UseFormReturn<{
    loginID: string;
    reCAPTCHAResponse: string;
    appName?: (typeof CLIENT_APPLICATION_LIST)[number];
}>;
const mockTolgee = {} as TolgeeInstance;
const mockRouterPush = jest.fn();

jest.mock('next-intl/navigation', () => ({
    createSharedPathnamesNavigation: () => ({
        useRouter: () => ({
            push: mockRouterPush,
        }),
    }),
}));
jest.mock('../../../../../trpc/server', () => ({
    api: {
        authentication: { forgotPassword: { mutate: () => {} } },
    },
}));
jest.mock('@tolgee/react', () => ({
    useTolgee: jest.fn().mockImplementation(() => mockTolgee),
})).mock('react-hook-form', () => ({
    useForm: jest.fn().mockImplementation(() => mockForgotPasswordForm),
}));

describe('forgot-password', () => {
    it('snapshot initial page', async () => {
        mockForgotPasswordForm.formState.isSubmitSuccessful = false;
        const fpp = ForgotPasswordPage();
        expect(fpp).toMatchSnapshot();
        expect(fpp.props.form).toBe(mockForgotPasswordForm);
        expect(fpp.props.tolgee).toBe(mockTolgee);
        fpp.props.onClickBackToLogin();
        expect(mockRouterPush).toHaveBeenCalledWith('login');
        const mockData = 'formSubmitMockData';
        api.authentication.forgotPassword.mutate = jest.fn();
        await fpp.props.formSubmitHandler(mockData);
        expect(api.authentication.forgotPassword.mutate).toHaveBeenCalledWith(mockData);
        const mockConsoleError = jest.spyOn(console, 'error').mockReturnValue();
        (api.authentication.forgotPassword.mutate as jest.Mock).mockImplementation(() => {
            throw new Error('mock');
        });
        await fpp.props.formSubmitHandler(mockData);
        expect(mockConsoleError).toHaveBeenCalledWith(new Error('mock'));
    });
    it('snapshot successfully submitted page', () => {
        mockForgotPasswordForm.formState.isSubmitSuccessful = true;
        const fpp = ForgotPasswordPage();
        expect(fpp).toMatchSnapshot();
        expect(fpp.props.form).toBe(mockForgotPasswordForm);
        expect(fpp.props.tolgee).toBe(mockTolgee);
        fpp.props.onClickBackToLogin();
        expect(mockRouterPush).toHaveBeenCalledWith('login');
    });
    afterEach(() => {
        jest.clearAllMocks();
    });
});
